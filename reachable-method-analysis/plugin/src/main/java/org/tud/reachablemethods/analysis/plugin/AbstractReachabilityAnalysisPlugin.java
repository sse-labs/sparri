package org.tud.reachablemethods.analysis.plugin;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.tud.reachablemethods.analysis.logging.AnalysisLogger;
import org.tud.reachablemethods.analysis.model.MavenIdentifier;
import org.tud.reachablemethods.analysis.model.MavenJarFileDependency;
import scala.Some;
import scala.collection.JavaConverters;

import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractReachabilityAnalysisPlugin extends AbstractMojo {

    @Parameter(defaultValue = "${project.build.directory}", required = true, readonly = true)
    protected File outputDirectory;

    @Parameter(defaultValue = "${project}", required = true, readonly = true)
    protected MavenProject project;

    @Parameter(defaultValue = "${project.basedir}", readonly = true)
    protected File basedir;


    protected Log log;
    protected AnalysisLogger analysisLogger;

    public AbstractReachabilityAnalysisPlugin() {
        this.log = getLog();
        this.analysisLogger = new AnalysisLoggerAdapter(this.log);
    }

    protected List<MavenJarFileDependency> getDependencies() {
        if(project != null) {
            return project
                    .getArtifacts()
                    .stream()
                    .filter( artifact -> !artifact.getScope().equals("test"))
                    .map( artifact -> new MavenJarFileDependency(new MavenIdentifier(artifact.getGroupId(), artifact.getArtifactId(), artifact.getVersion()),
                            artifact.getFile(),
                            new Some<>(artifact.getScope())))
                    .collect(Collectors.toList());
        } else {
            log.error("Project instance is not set.");
            return null;
        }
    }

    protected scala.collection.Iterable<MavenJarFileDependency> getDependenciesAsScalaList() {
        return  JavaConverters.collectionAsScalaIterable(getDependencies());
    }

    protected File getClassesRoot() {
        return Paths.get(outputDirectory.getAbsolutePath(), "classes").toFile();
    }

}
