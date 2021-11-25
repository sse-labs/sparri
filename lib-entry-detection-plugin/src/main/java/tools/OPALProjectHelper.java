package tools;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigValueFactory;
import org.apache.maven.plugin.logging.Log;
import org.opalj.br.ClassFile;
import org.opalj.br.analyses.InconsistentProjectException;
import org.opalj.br.analyses.Project;
import org.opalj.br.reader.Java16LibraryFramework;
import org.opalj.br.reader.Java16LibraryFramework$;
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
import java.util.Enumeration;
import java.util.List;

import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class OPALProjectHelper {

    private final Config opalBaseConfig = org.opalj.package$.MODULE$.BaseConfig();

    private final Log log;
    private final OPALLogger opalLogger;


    private final Java16LibraryFramework fullClassFileReader;
    private final Java16LibraryFramework interfaceClassFileReader;


    public OPALProjectHelper(Log pluginLog){
        // Build and register logger
        this.log = pluginLog;
        OPALLogAdapter logAdapter = OPALLogAdapter.buildPluginLogAdapter(pluginLog);
        this.opalLogger = logAdapter;
        logAdapter.setInfoLevelEnabled(false);
        LogContext opalLogCtx = new StandardLogContext();
        OPALLogger.register(opalLogCtx, logAdapter);
        OPALLogAdapter.updateGlobalContext(OPALLogAdapter.buildNullLogger());

        // Build class file reader after logging has been initialized
        this.fullClassFileReader = Project.JavaClassFileReader(opalLogCtx, opalBaseConfig);
        this.interfaceClassFileReader = Java16LibraryFramework$.MODULE$;
    }


    public Project<URL> buildOpalProject(String classFileRoot, List<Tuple2<ClassFile, URL>> thirdPartyClasses, boolean isLibrary, boolean completelyLoadLibraries){
        // Load the JRE with implementation
        List<Tuple2<ClassFile, URL>> jreClasses =
                readClassesFromClassContainerFiles(getAllJREClassContainers(), true);

        // Collect all library classes, combination of JRE and third party classes
        List<Tuple2<ClassFile, URL>> allLibraryClasses =
                Stream.concat(jreClasses.stream(), thirdPartyClasses.stream()).collect(Collectors.toList());

        // Convert library classes to scala
        scala.collection.Traversable<Tuple2<ClassFile, URL>> allLibraryClassesScala =
                JavaConverters.collectionAsScalaIterable(allLibraryClasses);

        // Always load implementation of actual project files
        //TODO: This does not deal with test classes yet, we might want to do that
        scala.collection.Traversable<Tuple2<ClassFile, URL>> projectFiles =
                JavaConverters.collectionAsScalaIterable(readProjectClassFiles(classFileRoot, true));

        // Empty list of virtual classfiles is desired behavior
        scala.collection.Traversable<ClassFile> virtualClassFiles =
                JavaConverters.collectionAsScalaIterable(new ArrayList<>());

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

    private List<File> getAllJREClassContainers(){
        List<File> jars = new ArrayList<>();
        File libDir = package$.MODULE$.JRELibraryFolder();

        if(libDir.exists() && libDir.isDirectory()){
            addJREClassContainersRecursive(libDir, jars);
        }

        return jars;
    }

    private List<ClassFile> buildClassFile(DataInputStream is, boolean loadImplementation){
        if(loadImplementation){
            return JavaConverters.seqAsJavaList(this.fullClassFileReader.ClassFile(is).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        } else {
            return JavaConverters.seqAsJavaList(this.interfaceClassFileReader.ClassFile(is).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        }
    }

    private List<ClassFile> buildClassFile(File jarFile, String entryName, boolean loadImplementation) throws IOException {
        if(loadImplementation){
            return JavaConverters.seqAsJavaList(this.fullClassFileReader.ClassFile(jarFile, entryName).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        } else {
            return JavaConverters.seqAsJavaList(this.interfaceClassFileReader.ClassFile(jarFile, entryName).toSeq())
                    .stream().map( o -> (ClassFile)o).collect(Collectors.toList());
        }
    }

    private void addJREClassContainersRecursive(File directory, List<File> files){
        for(File content: directory.listFiles()){
            if(content.isFile() &&
                    (content.getName().toLowerCase().endsWith(".jar") || content.getName().toLowerCase().endsWith(".jmod")) &&
                    !content.getName().equalsIgnoreCase("jfxswt.jar")){
                files.add(content);
            } else if(content.isDirectory()){
                addJREClassContainersRecursive(content, files);
            }
        }
    }

    private void addClassFilesRecursive(File directory, List<Tuple2<ClassFile, URL>> files, boolean loadImplementation) throws FileNotFoundException {
        for(File content : directory.listFiles()){
            if(content.isFile() && content.getName().toLowerCase().endsWith(".class")){
                buildClassFile(new DataInputStream(new FileInputStream(content)), loadImplementation)
                        .forEach(cf -> {
                            URL fileURL = getFileUrl(content);

                            files.add(Tuple2.apply(cf, fileURL));
                        });

            } else if(content.isDirectory()) {
                addClassFilesRecursive(content, files, loadImplementation);
            }
        }
    }

    private List<Tuple2<ClassFile, URL>> readProjectClassFiles(String classFileRoot, boolean loadImplementation){
        log.debug("Loading project class files from " + classFileRoot);

        File classFileDirectory = new File(classFileRoot);

        if(!classFileDirectory.exists() || !classFileDirectory.isDirectory()){
            throw new IllegalStateException("Project class files root is not a directory.");
        }

        List<Tuple2<ClassFile, URL>> classFiles = new ArrayList<>();

        try{
            addClassFilesRecursive(classFileDirectory, classFiles, loadImplementation);
            return classFiles;
        } catch(IOException iox){
            log.error("Error reading project class files", iox);
        }

        return null;
    }

    public List<Tuple2<ClassFile, URL>> readClassesFromJar(File jarFile, boolean loadImplementation) throws IOException {
        List<Tuple2<ClassFile, URL>> entries = new ArrayList<>();

        log.debug("Reading classes from JAR: " + jarFile.getName());
        JarInputStream in = new JarInputStream(new FileInputStream(jarFile));

        JarEntry currentEntry = in.getNextJarEntry();

        while(currentEntry != null){
            String entryName = currentEntry.getName();

            if(currentEntry.getName().toLowerCase().endsWith(".class")){
                buildClassFile(jarFile, entryName, loadImplementation)
                        .forEach(cf -> {
                            URL fileURL = getFileUrl(jarFile);

                            entries.add(Tuple2.apply(cf, fileURL));
                        });

            }

            currentEntry = in.getNextJarEntry();
        }

        in.close();

        return entries;
    }

    public List<Tuple2<ClassFile, URL>> readClassesFromJMod(File jmodFile, boolean loadImplementation) throws IOException {
        List<Tuple2<ClassFile, URL>> entries = new ArrayList<>();

        log.debug("Reading classes from JMOD: " + jmodFile.getName());

        ZipFile jmod = new ZipFile(jmodFile);
        Enumeration<? extends ZipEntry> zipEntryEnum = jmod.entries();

        while(zipEntryEnum.hasMoreElements()){
            ZipEntry currentEntry = zipEntryEnum.nextElement();
            String entryName = currentEntry.getName().toLowerCase();

            if(entryName.endsWith(".class")){
                InputStream is = jmod.getInputStream(currentEntry);
                DataInputStream dis = getEntryByteStream(is);
                buildClassFile(dis, loadImplementation).forEach(cf -> {
                    URL fileURL = getFileUrl(jmodFile);

                    entries.add(Tuple2.apply(cf, fileURL));
                });
            }

        }

        jmod.close();

        return entries;
    }

    public List<Tuple2<ClassFile, URL>> readClassesFromClassContainerFiles(List<File> containerFiles, boolean loadImplementation){
        List<Tuple2<ClassFile, URL>> entries = new ArrayList<>();

        try {
            for(File containerFile : containerFiles){
                if(containerFile.getName().toLowerCase().endsWith(".jar")){
                    entries.addAll(readClassesFromJar(containerFile, loadImplementation));
                } else if(containerFile.getName().toLowerCase().endsWith(".jmod")){
                    entries.addAll(readClassesFromJMod(containerFile, loadImplementation));
                } else {
                    log.error("Unknown class container file type that is not JAR or JMOD: " + containerFile.getName());
                }
            }

            return entries;

        } catch (IOException iox){
            log.error(iox);
            return null;
        }
    }


    private DataInputStream getEntryByteStream(InputStream is) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[32 * 1024];
        int bytesRead = is.read(buffer);

        while(bytesRead > 0){
            baos.write(buffer, 0, bytesRead);
            baos.flush();
            bytesRead = is.read(buffer);
        }

        byte[] entryBytes = baos.toByteArray();

        return new DataInputStream(new ByteArrayInputStream(entryBytes));
    }

}
