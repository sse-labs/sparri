package common;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import org.opalj.br.analyses.Project;

import tools.MavenOPALProjectWrapper;

import java.io.*;

import java.net.URL;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public abstract class OPALBasedAnalysisPlugin extends AbstractMojo {

    @Parameter(defaultValue = "${project.build.directory}", required = true, readonly = true)
    protected File outputDirectory;

    @Parameter(defaultValue = "${project}", required = true, readonly = true)
    protected MavenProject project;

    @Parameter(defaultValue = "${project.basedir}", readonly = true)
    protected File basedir;

    @Parameter(defaultValue = "false")
    protected Boolean isLibrary;

    @Parameter(defaultValue = "false")
    protected Boolean loadDependencyImplementation;

    @Parameter(defaultValue = "true")
    protected Boolean loadThirdPartyLibraries;



    protected Log log;

    private Project<URL> theOPALProjectInstance = null;

    protected final MavenOPALProjectWrapper opalProjectWrapper;

    public OPALBasedAnalysisPlugin(){
        this.log = getLog();
        this.opalProjectWrapper = new MavenOPALProjectWrapper(() -> this.project, this.log);
    }

    protected List<String> getAllDependencyNames(){
        if(project != null){
            return project.getArtifacts().stream()
                    .map( artifact -> artifact.getGroupId() + ":" + artifact.getArtifactId() + ":" + artifact.getVersion()).collect(Collectors.toList());
        } else {
            log.error("Project instance is not set.");
            return null;
        }
    }

    protected List<String> getDirectDependencyNames(){
        if(project != null){
            return project.getDependencies().stream()
                    .map(artifact -> artifact.getGroupId() + ":" + artifact.getArtifactId() + ":" + artifact.getVersion()).collect(Collectors.toList() );
        } else {
            log.error("Project instance is not set.");
            return null;
        }
    }

    protected Project<URL> getOPALProjectWithClassSources() {

        if(theOPALProjectInstance == null){
            log.debug("Initializing OPAL with the following configuration:");
            log.debug("\t- Root project is libary: " + this.isLibrary);
            log.debug("\t- Load implementation of 3rd party libraries: " + this.loadDependencyImplementation);
            log.debug("\t- Load 3rd party libraries: " + this.loadThirdPartyLibraries);

            this.opalProjectWrapper.initializeOPALProject(getClassSourceDirectory(), this.isLibrary,
                    this.loadDependencyImplementation, this.loadThirdPartyLibraries);
            this.theOPALProjectInstance = this.opalProjectWrapper.getOPALProject();
        }

        return theOPALProjectInstance;
    }

    protected String getFilePath(String first, String... more) {
        return Paths.get(first, more).toString();
    }

    private String getClassSourceDirectory() {
        return getFilePath(outputDirectory.getAbsolutePath(), "classes");
    }


}
