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
import scala.collection.immutable.Set;
import scala.util.Try;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

@Mojo( name = "compare-reachability-analyses", requiresDependencyResolution = ResolutionScope.RUNTIME, requiresDependencyCollection = ResolutionScope.RUNTIME, defaultPhase = LifecyclePhase.PREPARE_PACKAGE)
public class ReachabilityAnalysisComparisonPlugin extends AbstractReachabilityAnalysisPlugin {

    @Parameter(defaultValue = "false", required = false)
    protected boolean projectIsLibrary;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {


        log.info("Starting Compositional Method Reachability Analysis...");
        ReachabilityAnalysis compositionalReachabilityAnalysis = new CompositionalReachabilityAnalysis(new Configuration(), analysisLogger);

        long compositionalStart = System.currentTimeMillis();
        Try<Set<String>> compositionalReachableMethods = compositionalReachabilityAnalysis.analyzeMavenProject(getClassesRoot(),
                getDependenciesAsScalaList(), projectIsLibrary);
        long compositionalDuration = (System.currentTimeMillis() - compositionalStart) / 1000L;

        if(compositionalReachableMethods.isFailure()){
            log.error("Compositional Method Reachability Analysis failed in " + compositionalDuration + " s.",
                    compositionalReachableMethods.failed().get());
        } else {
            log.info("Compositional Method Reachability Analysis completed in " + compositionalDuration + " s.");
        }

        log.info("Starting Regular OPAL Method Reachability Analysis...");
        ReachabilityAnalysis regularReachabilityAnalysis = new RegularOpalReachabilityAnalysis(analysisLogger);

        long regularStart = System.currentTimeMillis();
        Try<Set<String>> regularReachableMethods = regularReachabilityAnalysis.analyzeMavenProject(getClassesRoot(),
                getDependenciesAsScalaList(), projectIsLibrary);
        long regularDuration = (System.currentTimeMillis() - regularStart)  / 1000L;

        if(regularReachableMethods.isFailure()){
            log.error("Compositional Method Reachability Analysis failed in " + regularDuration + " s.",
                    regularReachableMethods.failed().get());
        } else {
            log.info("Compositional Method Reachability Analysis completed in " + regularDuration + " s.");
        }

        if(compositionalReachableMethods.isSuccess() && regularReachableMethods.isSuccess()){
            int regularHits = regularReachableMethods.get().size();
            int compositionalHits = compositionalReachableMethods.get().size();

            File onlyInRegularFile = Paths.get(basedir.getPath(), "unique-regular-results.txt").toFile();
            File onlyInCompositionalFile = Paths.get(basedir.getPath(), "unique-compositional-results.txt").toFile();
            File commonHitsFile = Paths.get(basedir.getPath(), "common-results.txt").toFile();
            File durationsFile = Paths.get(basedir.getPath(), "durations.txt").toFile();

            List<String> onlyInRegular = new ArrayList<>(JavaConverters.seqAsJavaList(regularReachableMethods.get()
                    .filterNot(s -> compositionalReachableMethods.get().contains(s)).toSeq()));
            List<String> onlyInCompositional = new ArrayList<>(JavaConverters.seqAsJavaList(compositionalReachableMethods.get()
                    .filterNot(s -> regularReachableMethods.get().contains(s)).toSeq()));
            List<String> commonHits = new ArrayList<>(JavaConverters.seqAsJavaList(compositionalReachableMethods.get().intersect(regularReachableMethods.get()).toList()));


            log.info("Regular analysis yielded " + regularHits + " results ( " + onlyInRegular.size() + " unique ).");
            log.info("Compositional analysis yielded " + compositionalHits + " results ( " + onlyInCompositional.size() + " unique ).");

            List<String> durations = new ArrayList<>();
            durations.add("Regular=" + regularDuration + "s");
            durations.add("Compositional=" + compositionalDuration + "s");
            try {
                Files.write(onlyInRegularFile.toPath(), onlyInRegular);
                Files.write(onlyInCompositionalFile.toPath(), onlyInCompositional);
                Files.write(commonHitsFile.toPath(), commonHits);
                Files.write(durationsFile.toPath(), durations);
            } catch (IOException iox) {
                log.error("Failed to write files", iox);
            }

        } else {
            log.error("Not writing results as some analyses failed.");
        }

    }

}
