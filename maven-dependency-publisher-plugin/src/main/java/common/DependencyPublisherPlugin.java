package common;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import storage.MessageQueuePublisher;


import java.io.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Mojo( name = "publish-dependencies", requiresDependencyResolution = ResolutionScope.RUNTIME, requiresDependencyCollection = ResolutionScope.RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class DependencyPublisherPlugin extends AbstractMojo {

    @Parameter(defaultValue = "${project}", required = true, readonly = true)
    protected MavenProject project;

    @Parameter(defaultValue = "${project.basedir}", readonly = true)
    protected File basedir;

    @Parameter(defaultValue = "")
    protected String mqHost;

    @Parameter(defaultValue = "")
    protected String mqUser;

    @Parameter(defaultValue = "")
    protected String mqPassword;



    protected Log log;

    public DependencyPublisherPlugin(){
        this.log = getLog();
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

     @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        List<String> dependencies = getAllDependencyNames();
        dependencies.add("<none>:<jre>:<none>"); //Always add JRE

         List<String> libraries = new ArrayList<>();

         for(String dep: dependencies){
             String libIdent = dep.substring(0, dep.lastIndexOf(":"));
             if(!libraries.contains(libIdent)){
                 libraries.add(dep);
             }
         }

         MessageQueuePublisher publisher = null;

         try{
             publisher = new MessageQueuePublisher(new Configuration(this.mqUser, this.mqPassword, this.mqHost));
             publisher.initialize();

             for(String lib: libraries){
                 log.info("Publishing identifier: \"" + lib + "\"");
                 publisher.publishIdentifier(lib);
             }

             publisher.shutdown();
         } catch(Exception iox){
            log.error("Failure while publishing dependencies: ", iox);
         } finally {
             if(publisher != null) {
                 try { publisher.shutdown(); } catch (IOException iox) { log.error("Error closing connection", iox); }
             }
         }

         log.info("Done publishing " + libraries.size() + " dependency libraries for " +
                 project.getGroupId() + ":" + project.getArtifactId() + ":" + project.getVersion());
    }


}
