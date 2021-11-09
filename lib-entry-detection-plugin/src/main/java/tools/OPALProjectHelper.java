package tools;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigValueFactory;
import org.apache.maven.plugin.logging.Log;
import org.opalj.br.ClassFile;
import org.opalj.br.analyses.InconsistentProjectException;
import org.opalj.br.analyses.Project;
import org.opalj.br.reader.Java8LibraryFramework;
import org.opalj.br.reader.Java9Framework$;
import org.opalj.bytecode.package$;
import org.opalj.log.LogContext;
import org.opalj.log.OPALLogger;
import org.opalj.log.StandardLogContext;
import scala.Function2;
import scala.Tuple2;
import scala.collection.JavaConverters;
import scala.runtime.AbstractFunction2;
import scala.runtime.BoxedUnit;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OPALProjectHelper {

    private final Config opalBaseConfig = org.opalj.package$.MODULE$.BaseConfig();

    private final Java8LibraryFramework reader;

    private final Log log;
    private final OPALLogger opalLogger;


    public OPALProjectHelper(Log pluginLog){
        this.log = pluginLog;

        OPALLogAdapter logAdapter = OPALLogAdapter.buildPluginLogAdapter(pluginLog);
        this.opalLogger = logAdapter;

        logAdapter.setInfoLevelEnabled(false);
        LogContext opalLogCtx = new StandardLogContext();
        OPALLogger.register(opalLogCtx, logAdapter);
        OPALLogAdapter.updateGlobalContext(OPALLogAdapter.buildNullLogger());

        this.reader = Project.JavaClassFileReader(opalLogCtx, this.opalBaseConfig);
    }

    public Project<URL> buildOpalProjectWithJars(String classFileRoot, List<File> thirdPartyJars, boolean isLibrary, boolean completelyLoadLibraries) {
        return buildOpalProject(classFileRoot, readClassesFromJar(thirdPartyJars, completelyLoadLibraries), isLibrary, completelyLoadLibraries);
    }


    public Project<URL> buildOpalProject(String classFileRoot, List<Tuple2<ClassFile, URL>> thirdPartyClasses, boolean isLibrary, boolean completelyLoadLibraries){
        List<Tuple2<ClassFile, URL>> jreClasses = readClassesFromJar(getAllJREJarFiles(), completelyLoadLibraries);
        List<Tuple2<ClassFile, URL>> allLibraryClasses = Stream.concat(jreClasses.stream(), thirdPartyClasses.stream()).collect(Collectors.toList());
        scala.collection.Traversable<Tuple2<ClassFile, URL>> allLibraryClassesScala = JavaConverters.collectionAsScalaIterable(allLibraryClasses);

        //TODO: This does not deal with test classes yet, we might want to do that
        scala.collection.Traversable<Tuple2<ClassFile, URL>> projectFiles = JavaConverters.collectionAsScalaIterable(readProjectClassFiles(classFileRoot, completelyLoadLibraries));
        // Empty list of virtual classfiles is desired behavior
        scala.collection.Traversable<ClassFile> virtualClassFiles = JavaConverters.collectionAsScalaIterable(new ArrayList<>());

        Config theConfig = this.opalBaseConfig;

        if(isLibrary){
            theConfig = theConfig.withValue("org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
                    ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder"));
        }

        Function2<LogContext, InconsistentProjectException, BoxedUnit> inconsistentHandler = new AbstractFunction2<LogContext, InconsistentProjectException, BoxedUnit>() {
            @Override
            public BoxedUnit apply(LogContext v1, InconsistentProjectException v2) {
                log.error("Inconsistent Project Error: " + v2.message());
                return BoxedUnit.UNIT;
            }
        };

        return Project.apply(projectFiles, allLibraryClassesScala, !completelyLoadLibraries, virtualClassFiles, inconsistentHandler, theConfig, opalLogger);
    }

    private URL getFileUrl(File file) {
        try {
            return file.toURI().toURL();
        } catch(MalformedURLException mux){
            log.error(mux);
            return null;
        }
    }

    private List<File> getAllJREJarFiles(){
        List<File> jars = new ArrayList<>();
        File libDir = package$.MODULE$.JRELibraryFolder();

        if(libDir.exists() && libDir.isDirectory()){
            addJarFilesRecursive(libDir, jars);
        }

        return jars;
    }

    private List<ClassFile> buildClassFiles(DataInputStream is, boolean completelyLoadLibaries){
        if(completelyLoadLibaries){
            return JavaConverters.seqAsJavaList(this.reader.ClassFile(is).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        } else {
            return JavaConverters.seqAsJavaList(Java9Framework$.MODULE$.ClassFile(is).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        }
    }

    private List<ClassFile> buildClassFile(File jarFile, String entryName, boolean completelyLoadLibaries) throws IOException {
        if(completelyLoadLibaries){
            return JavaConverters.seqAsJavaList(this.reader.ClassFile(jarFile, entryName).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        } else {
            return JavaConverters.seqAsJavaList(Java9Framework$.MODULE$.ClassFile(jarFile, entryName).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        }
    }

    private void addJarFilesRecursive(File directory, List<File> files){
        for(File content: directory.listFiles()){
            if(content.isFile() && content.getName().toLowerCase().endsWith(".jar") && !content.getName().equalsIgnoreCase("jfxswt.jar")){
                files.add(content);
            } else if(content.isDirectory()){
                addJarFilesRecursive(content, files);
            }
        }
    }

    private void addClassFilesRecursive(File directory, List<Tuple2<ClassFile, URL>> files, boolean completelyLoadClassFiles) throws FileNotFoundException {
        for(File content : directory.listFiles()){
            if(content.isFile() && content.getName().toLowerCase().endsWith(".class")){
                buildClassFiles(new DataInputStream(new FileInputStream(content)), completelyLoadClassFiles)
                        .forEach(cf -> {
                            URL fileURL = getFileUrl(content);

                            files.add(Tuple2.apply(cf, fileURL));
                        });

            } else if(content.isDirectory()) {
                addClassFilesRecursive(content, files, completelyLoadClassFiles);
            }
        }
    }

    private List<Tuple2<ClassFile, URL>> readProjectClassFiles(String classFileRoot, boolean completelyLoadLibraries){
        log.debug("Loading project class files from " + classFileRoot);

        File classFileDirectory = new File(classFileRoot);

        if(!classFileDirectory.exists() || !classFileDirectory.isDirectory()){
            throw new IllegalStateException("Project class files root is not a directory.");
        }

        List<Tuple2<ClassFile, URL>> classFiles = new ArrayList<>();

        try{
            addClassFilesRecursive(classFileDirectory, classFiles, completelyLoadLibraries);
            return classFiles;
        } catch(IOException iox){
            log.error("Error reading project class files", iox);
        }

        return null;
    }

    public List<Tuple2<ClassFile, URL>> readClassesFromJar(List<File> jarFiles, boolean completelyLoadLibraries){
        List<Tuple2<ClassFile, URL>> entries = new ArrayList<>();

        try {
            for(File jarFile : jarFiles){
                log.debug("Reading classes from JAR: " + jarFile.getName());
                JarInputStream in = new JarInputStream(new FileInputStream(jarFile));

                JarEntry currentEntry = in.getNextJarEntry();

                while(currentEntry != null){
                    String entryName = currentEntry.getName();

                    if(currentEntry.getName().toLowerCase().endsWith(".class")){
                        buildClassFile(jarFile, entryName, completelyLoadLibraries)
                                .forEach(cf -> {
                                    URL fileURL = getFileUrl(jarFile);

                                    entries.add(Tuple2.apply(cf, fileURL));
                                });

                    }

                    currentEntry = in.getNextJarEntry();
                }

                in.close();
            }

            return entries;

        } catch (IOException iox){
            log.error(iox);
            return null;
        }
    }




}
