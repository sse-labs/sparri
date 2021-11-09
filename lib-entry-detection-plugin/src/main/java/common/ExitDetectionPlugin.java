package common;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.opalj.br.ClassFile;
import org.opalj.br.DeclaredMethod;
import org.opalj.br.ObjectType;
import org.opalj.br.analyses.Project;
import org.opalj.tac.cg.CallGraph;
import org.opalj.tac.cg.RTACallGraphKey$;
import org.opalj.tac.cg.XTACallGraphKey$;

import scala.collection.JavaConverters;

import java.io.BufferedWriter;
import java.io.File;
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

    private void addAllCallees(DeclaredMethod currentMethod, CallGraph cg, Set<DeclaredMethod> allMethods) {

        //TODO: Performance: Maybe we can cutoff recursive exploration if we enter a Java Runtime Method, as we can assume
        //TODO: calls to the JRE to be fully resolved and not invoking external libs

        List<DeclaredMethod> allCallees = asJavaList(cg.calleesOf(currentMethod))
                .stream()
                .flatMap(t -> asJavaList(t._2).stream())
                .collect(Collectors.toList());
        allCallees.forEach(method -> {
            if(!allMethods.contains(method)){
                allMethods.add(method);
                addAllCallees(method, cg, allMethods);
            }
        });
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

        log.info("Building XTA Callgraph...");
        CallGraph theCg = project.get(XTACallGraphKey$.MODULE$);
        log.info("Done building XTA Callgraph.");

        List<String> dependencies = getAllDependencyNames();
        String dependencyListFilePath = getDependencyListFilePath();
        writeToFile(dependencyListFilePath, dependencies);
        log.info("Wrote " + dependencies.size() +
                " total dependencies to file '" + dependencyListFilePath + "'");

        Set<DeclaredMethod> allMethods = new HashSet<>();
        Set<ObjectType> allProjectInternalTypes = allProjectAndLibraryTypes();

        for(DeclaredMethod entry: asJavaList(theCg.reachableMethods())){
            addAllCallees(entry, theCg, allMethods);
        }

        // TODO: Maybe test for methods instead of project types?
        List<String> externalMethodCallNames = allMethods
                .stream()
                .flatMap( method -> {
                    if(this.opalProjectWrapper.isProjectType(method.declaringClassType())){

                        List<DeclaredMethod> thirdPartyInvocations =  JavaConverters.seqAsJavaList(theCg.calleesOf(method).flatMap( m -> m._2)
                                .filter( m ->
                                    (this.loadThirdPartyLibraries && this.opalProjectWrapper.isThirdPartyType(m.declaringClassType())) ||
                                            (!this.loadThirdPartyLibraries && !allProjectInternalTypes.contains(m.declaringClassType()))
                                ).toSeq());

                        if(thirdPartyInvocations.size() > 0){
                            log.info("Project method '" + method.toJava() + "' invokes " + thirdPartyInvocations.size() + " 3rd party methods: " + thirdPartyInvocations.get(0).toJava());
                        }

                        return thirdPartyInvocations.stream();
                    } else {
                        return Stream.empty();
                    }
                })
                .distinct()
                //.filter( method -> (!this.loadThirdPartyLibraries && !allProjectInternalTypes.contains(method.declaringClassType())) ||
                //        (this.loadThirdPartyLibraries && this.opalProjectWrapper.isThirdPartyType(method.declaringClassType())))
                //.filter( m -> this.opalProjectWrapper.isThirdPartyType(m.declaringClassType()))
                .map(DeclaredMethod::toJava)
                .collect(Collectors.toList());

        String libEntryListFilePath = getLibEntryListFilePath();
        writeToFile(libEntryListFilePath, externalMethodCallNames);
        log.info("Wrote " + externalMethodCallNames.size() +
                " total library entry calls to file '" + libEntryListFilePath + "'");
    }


}
