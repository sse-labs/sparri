package common;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.opalj.Result;
import org.opalj.br.ClassFile;
import org.opalj.br.Method;
import org.opalj.br.ObjectType;
import org.opalj.br.analyses.Project;
import org.opalj.br.analyses.cg.InitialEntryPointsKey$;
import org.opalj.br.instructions.INVOKEINTERFACE;
import org.opalj.br.instructions.INVOKESPECIAL;
import org.opalj.br.instructions.INVOKESTATIC;
import org.opalj.br.instructions.INVOKEVIRTUAL;

import scala.collection.JavaConverters;
import scala.collection.Traversable;
import scala.runtime.BoxedUnit;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Mojo( name = "detect-exits", requiresDependencyResolution = ResolutionScope.RUNTIME, requiresDependencyCollection = ResolutionScope.RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class ExitDetectionPlugin extends OPALBasedAnalysisPlugin {

    private String getDependencyListFilePath(){
        return Paths.get(super.basedir.getPath(), "all_deps.txt").toString();
    }

    private String getLibEntryListFilePath(){
        return Paths.get(super.basedir.getPath(), "lib_entry_calls.txt").toString();
    }

    private <T> List<T> asJavaList(scala.collection.Iterator<T> it){
        return JavaConverters.seqAsJavaListConverter(it.toSeq()).asJava();
    }


    private Set<ObjectType> allProjectAndLibraryTypes() {
        List<ClassFile> allClassFiles = new ArrayList<>(JavaConverters.seqAsJavaList(getOPALProjectWithClassSources().allProjectClassFiles().toSeq()));
        allClassFiles.addAll(JavaConverters.seqAsJavaList(getOPALProjectWithClassSources().allLibraryClassFiles().toSeq()));

        return allClassFiles
                .stream()
                .map(ClassFile::thisType)
                .collect(Collectors.toSet());
    }

    private void writeToFile(String fileName, List<String> lines) {
        try{
            BufferedWriter writer = new BufferedWriter(new FileWriter(fileName));

            for(String line:  lines){
                writer.write(line + System.lineSeparator());
            }

            writer.flush();
            writer.close();
        } catch (IOException iox) {
            log.error("Failed to write result file '" + fileName + "'");
        }

    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        log.info("Building OPAL project instance...");
        Project<URL> project = getOPALProjectWithClassSources();
        log.info("Done building OPAL project.");


        log.info("Building custom callgraph...");
        Traversable<Method> entryPoints = project.get(InitialEntryPointsKey$.MODULE$)
                .filter(m -> project.isProjectType(m.classFile().thisType()) && m.body().isDefined());

        entryPoints.foreach(e -> {log.info("ENTRY: " + e.fullyQualifiedSignature()); return BoxedUnit.UNIT;});

        Set<String> signaturesSeen = new HashSet<>();
        Map<Method, Integer> externalMethodInvocations = new HashMap<>();

        entryPoints.foreach(m -> {processProjectMethod(m, signaturesSeen, project, externalMethodInvocations); return BoxedUnit.UNIT;});


        log.info("Done building callgraph.");
        //Now externalMethodInvocations contains a mapping of all 3rd party entry points to the number of invocations


        List<String> dependencies = getAllDependencyNames();
        String dependencyListFilePath = getDependencyListFilePath();
        writeToFile(dependencyListFilePath, dependencies);
        log.info("Wrote " + dependencies.size() +
                " total dependencies to file '" + dependencyListFilePath + "'");

        List<String> externalMethodCallNames = new ArrayList<>();

        for(Method externalMethod : externalMethodInvocations.keySet()){
            externalMethodCallNames.add( externalMethod.fullyQualifiedSignature() + " (invoked: " + externalMethodInvocations.get(externalMethod) + " times)");
        }

        String libEntryListFilePath = getLibEntryListFilePath();
        writeToFile(libEntryListFilePath, externalMethodCallNames);
        log.info("Wrote " + externalMethodCallNames.size() +
                " total library entry calls to file '" + libEntryListFilePath + "'");
    }



    private void processProjectMethod(Method method, Set<String> signaturesSeen,
                                      Project<URL> opalProject, Map<Method, Integer> externalMethodInvocations){
        signaturesSeen.add(method.fullyQualifiedSignature());
        //if(signaturesSeen.size() % 4000 == 0) log.info("Processing method #" + signaturesSeen.size());
        if(method.body().isDefined()){
            getAllCallees(method, opalProject)
                    .forEach( callee -> {
                        if(!signaturesSeen.contains(callee.fullyQualifiedSignature())&&
                                opalProject.isProjectType(callee.classFile().thisType())){
                            processProjectMethod(callee, signaturesSeen, opalProject, externalMethodInvocations);
                        } else if(opalProjectWrapper.isThirdPartyType(callee.classFile().thisType())){

                            if(externalMethodInvocations.containsKey(callee)){
                                externalMethodInvocations.put(callee, externalMethodInvocations.get(callee) + 1);
                            } else {
                                externalMethodInvocations.put(callee, 1);
                            }
                        }
                    });
        }
    }

    private List<Method> getAllCallees(Method method, Project<URL> project) {
        assert(method.body().isDefined());

        return Arrays.stream(method.body().get().instructions())
                .filter(i -> i != null && i.isMethodInvocationInstruction())
                .flatMap(instr -> {
                    if(instr instanceof INVOKEVIRTUAL){
                        INVOKEVIRTUAL iv = (INVOKEVIRTUAL) instr;
                        return JavaConverters.setAsJavaSet(project.virtualCall(method.classFile().thisType(), iv)).stream();
                    } else if (instr instanceof INVOKEINTERFACE){
                        INVOKEINTERFACE inf = (INVOKEINTERFACE) instr;
                        return JavaConverters.setAsJavaSet(project.interfaceCall(method.classFile().thisType(), inf)).stream();
                    } else if (instr instanceof INVOKESPECIAL){
                        INVOKESPECIAL is = (INVOKESPECIAL) instr;
                        Result<Method> result = project.specialCall(method.classFile().thisType(), is);

                        if(result.isEmpty()){
                            log.warn("Failed to resolve special call " + is);
                            log.warn("\t- Call contained in " + method.fullyQualifiedSignature());
                            return Stream.empty();
                        } else {
                            return Stream.of(result.value());
                        }

                    } else if (instr instanceof INVOKESTATIC) {
                        INVOKESTATIC s = (INVOKESTATIC) instr;
                        Result<Method> result = project.staticCall(method.classFile().thisType(), s);

                        if(result.isEmpty()){
                            log.warn("Failed to resolve static call " + s);
                            log.warn("\t- Call contained in " + method.fullyQualifiedSignature());
                            return Stream.empty();
                        } else {
                            return Stream.of(result.value());
                        }
                    } else {
                        throw new IllegalStateException("Unknown method invocation instruction type: " + instr);
                    }
                })
                .collect(Collectors.toList());
    }


}
