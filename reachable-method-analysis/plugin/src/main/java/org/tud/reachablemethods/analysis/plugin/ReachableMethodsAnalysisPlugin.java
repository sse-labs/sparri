package org.tud.reachablemethods.analysis.plugin;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.tud.reachablemethods.analysis.Configuration;
import org.tud.reachablemethods.analysis.ReachabilityAnalysis;
import org.tud.reachablemethods.analysis.impl.CompositionalReachabilityAnalysis;
import org.tud.reachablemethods.analysis.impl.RegularOpalReachabilityAnalysis;

import scala.collection.JavaConverters;
import scala.util.Try;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

@Mojo( name = "detect-reachable-methods", requiresDependencyResolution = ResolutionScope.RUNTIME, requiresDependencyCollection = ResolutionScope.RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class ReachableMethodsAnalysisPlugin extends AbstractReachabilityAnalysisPlugin {

    @Parameter(defaultValue = "true", required = false)
    protected boolean useCompositionalAnalysis;

    @Parameter(defaultValue = "false", required = false)
    protected boolean projectIsLibrary;

    private File getOutputFile() {
        return Paths.get(basedir.getPath(), "reachable-methods.txt").toFile();
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        ReachabilityAnalysis theAnalysis;

        if(useCompositionalAnalysis) {
            theAnalysis = new CompositionalReachabilityAnalysis(new Configuration(), analysisLogger);
            log.info("Starting Compositional Method Reachability Analysis...");
        } else {
            theAnalysis = new RegularOpalReachabilityAnalysis(analysisLogger);
            log.info("Starting Regular OPAL Method Reachability Analysis...");
        }

        long startTime = System.currentTimeMillis();

        Try<scala.collection.immutable.Set<String>> result =
                theAnalysis.analyzeMavenProject(getClassesRoot(), getDependenciesAsScalaList(), projectIsLibrary);

        long duration = (System.currentTimeMillis() - startTime) / 1000L;

        if(result.isFailure()){
            log.error("Reachability analysis failed after " + duration + " seconds.");
            log.error("Error during analysis", result.failed().get());
        } else {
            log.info("Finished reachability analysis in " + duration + " seconds.");
            int hits = result.get().size();

            log.info("Got " + hits + " reachable methods. Writing results to " + getOutputFile().getName() + " ...");

            File outFile = getOutputFile();

            try {
                List<String> lines = new ArrayList<>(JavaConverters.seqAsJavaList(result.get().toSeq()));
                lines.add("Duration=" + duration +"s");

                Files.write(outFile.toPath(), lines);
                log.info("Successfully wrote results to file " + outFile.toPath());
            } catch(IOException iox){
                log.error("Failed to write reachable methods to file.", iox);
            }
        }

    }


}

