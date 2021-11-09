package tools;

import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.opalj.br.ClassFile;
import org.opalj.br.ObjectType;
import org.opalj.br.analyses.Project;
import scala.Tuple2;
import scala.collection.JavaConverters;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class MavenOPALProjectWrapper {

    private final  Supplier<MavenProject> theMavenProjectCreator;

    private Project<URL> theOPALProject = null;
    private final OPALProjectHelper opalHelper;

    private List<ObjectType> thirdPartyTypes = null;

    public MavenOPALProjectWrapper(Supplier<MavenProject> projectCreator, Log projectLog){
        this.theMavenProjectCreator = projectCreator;

        this.opalHelper = new OPALProjectHelper(projectLog);
    }

    public boolean isThirdPartyType(ObjectType type){
        return this.thirdPartyTypes != null && this.thirdPartyTypes.contains(type);
    }

    public boolean isProjectType(ObjectType type){
        return this.theOPALProject != null &&
                JavaConverters.seqAsJavaList(this.theOPALProject.allProjectClassFiles().toSeq()).stream()
                    .map(ClassFile::thisType)
                    .collect(Collectors.toList())
                    .contains(type);
    }

    public void initializeOPALProject(String projectClassFilesRoot, boolean rootProjectIsLibrary, boolean completelyLoadLibraryJars, boolean includeThirdPartyJars){

        if(this.theOPALProject == null){

            List<File> thirdPartyJarsToProcess = new ArrayList<>();

            if(includeThirdPartyJars){
                thirdPartyJarsToProcess.addAll(getThirdPartyLibraryJars());
            }

            List<Tuple2<ClassFile, URL>> thirdPartyClasses = this.opalHelper.readClassesFromJar(thirdPartyJarsToProcess, completelyLoadLibraryJars);

            if(includeThirdPartyJars){
                // Only set this if include3rdParty flag is set, value of null indicates opposite
                this.thirdPartyTypes = thirdPartyClasses.stream().map( t -> ((ClassFile)t._1).thisType()).collect(Collectors.toList());
            }

            this.theOPALProject = this.opalHelper.buildOpalProject(projectClassFilesRoot, thirdPartyClasses, rootProjectIsLibrary, completelyLoadLibraryJars);
        } else {
            throw new IllegalStateException("OPAL project has already been initialized.");
        }
    }

    public Project<URL> getOPALProject(){
        if(this.theOPALProject != null){
            return this.theOPALProject;
        } else {
            throw new IllegalStateException("OPAL project has not been initialized.");
        }
    }


    private List<File> getThirdPartyLibraryJars(){
        MavenProject project = this.theMavenProjectCreator.get();

        if(project != null){

            return project
                    .getArtifacts()
                    .stream()
                    .map( artefact -> {

                        if(artefact.isResolved() && !artefact.getFile().getPath().isEmpty()){
                            return artefact.getFile();
                        }

                        return null;
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }

        return null;
    }

}
