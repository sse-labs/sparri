package tools;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigValueFactory;
import org.apache.maven.plugin.logging.Log;
import org.opalj.br.ClassFile;
import org.opalj.br.analyses.InconsistentProjectException;
import org.opalj.br.analyses.Project;
import org.opalj.br.reader.Java8LibraryFramework;
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
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

public class OPALProjectHelper {

    private final File[] javaLibraryFiles = new File[]{
            package$.MODULE$.RTJar(), // The JRE RT.jar file
            Paths.get(package$.MODULE$.JRELibraryFolder().toPath().toString(), "ext", "jfxrt.jar").toFile(), // The JavaFX RT file
            Paths.get(package$.MODULE$.JRELibraryFolder().toPath().toString(), "jce.jar").toFile() // The crypto API
    };

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

    public Project<URL> buildOpalProject(String classFileRoot, boolean isLibrary){
        scala.collection.Traversable<Tuple2<ClassFile, URL>> libFiles = JavaConverters.collectionAsScalaIterable(readLibraryClassFiles());
        //TODO: This does not deal with test classes yet, we might want to do that
        scala.collection.Traversable<Tuple2<ClassFile, URL>> projectFiles = JavaConverters.collectionAsScalaIterable(readProjectClassFiles(classFileRoot));
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
                log.error(v2);
                return BoxedUnit.UNIT;
            }
        };

        return Project.apply(projectFiles, libFiles, true, virtualClassFiles, inconsistentHandler, theConfig, opalLogger);
    }

    private URL getFileUrl(File file) {
        try {
            return file.toURI().toURL();
        } catch(MalformedURLException mux){
            log.error(mux);
            return null;
        }
    }

    private void addClassFilesRecursive(File directory, List<Tuple2<ClassFile, URL>> files) throws FileNotFoundException {
        for(File content : directory.listFiles()){
            if(content.isFile() && content.getName().toLowerCase().endsWith(".class")){

                JavaConverters.seqAsJavaList(this.reader.ClassFile(new DataInputStream(new FileInputStream(content))).toSeq())
                        .forEach(cf -> {
                            URL fileURL = getFileUrl(content);

                            files.add(Tuple2.apply((ClassFile)cf, fileURL));
                        });

            } else if(content.isDirectory()) {
                addClassFilesRecursive(content, files);
            }
        }
    }

    private List<Tuple2<ClassFile, URL>> readProjectClassFiles(String classFileRoot){
        log.debug("Loading project class files from " + classFileRoot);

        File classFileDirectory = new File(classFileRoot);

        if(!classFileDirectory.exists() || !classFileDirectory.isDirectory()){
            throw new IllegalStateException("Project class files root is not a directory.");
        }

        List<Tuple2<ClassFile, URL>> classFiles = new ArrayList<>();

        try{
            addClassFilesRecursive(classFileDirectory, classFiles);
            return classFiles;
        } catch(IOException iox){
            log.error("Error reading project class files", iox);
        }

        return null;
    }


    private List<Tuple2<ClassFile, URL>> readLibraryClassFiles(){
        List<Tuple2<ClassFile, URL>> entries = new ArrayList<>();

        try {

            for(File libFile : this.javaLibraryFiles){
                log.debug("Processing library file " + libFile.getName());
                JarInputStream in = new JarInputStream(new FileInputStream(libFile));

                JarEntry currentEntry = in.getNextJarEntry();

                while(currentEntry != null){
                    String entryName = currentEntry.getName();

                    if(currentEntry.getName().toLowerCase().endsWith(".class")){
                        JavaConverters.seqAsJavaList(this.reader.ClassFile(libFile, entryName).toSeq())
                                .forEach(cf -> {
                                    URL fileURL = getFileUrl(libFile);

                                    entries.add(Tuple2.apply((ClassFile)cf, fileURL));
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
