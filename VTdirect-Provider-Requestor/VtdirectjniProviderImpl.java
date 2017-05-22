/* The following method VtdirectProviderImpl.java is an analysis provider that provides
VTdirect as a service over the SORCER network, and is implemented in accordance
with principles of exertion-oriented programming (EOP) as described in Chapter 3.2. */
/*
 * Distribution Statement
 * 
 * This computer software has been developed under sponsorship of the United States Air Force Research Lab. Any further
 * distribution or use by anyone or any data contained therein, unless otherwise specifically provided for,
 * is prohibited without the written approval of AFRL/RQVC-MSTC, 2210 8th Street Bldg 146, Room 218, WPAFB, OH  45433
 * 
 * Disclaimer
 * 
 * This material was prepared as an account of work sponsored by an agency of the United States Government. Neither
 * the United States Government nor the United States Air Force, nor any of their employees, makes any warranty,
 * express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness
 * of any information, apparatus, product, or process disclosed, or represents that its use would not infringe privately
 * owned rights.
 */
package mil.afrl.mstc.open.vtdirectjni.provider;

import com.sun.jini.start.LifeCycle;
import mil.afrl.mstc.open.vtdirectjni.Vtdirectjni;
import mil.afrl.mstc.open.vtdirectjni.VtdirectjniContext;
import mil.afrl.mstc.open.core.provider.EngineeringProvider;
import sorcer.service.Context;
import sorcer.util.GenericUtil;

import java.io.File;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Implementation of the {@link mil.afrl.mstc.open.vtdirectjni.Vtdirectjni} interface
 *
 * @author MSTC Engineering Project Generator
 */
public class VtdirectjniProviderImpl extends EngineeringProvider implements Vtdirectjni {
    private final static Logger logger = Logger.getLogger(VtdirectjniProviderImpl.class.getName());

    /**
     * Default constructor, initializes the provider with the properties file loaded as a resource.
     *
     * @throws Exception If initialization fails.
     */
    public VtdirectjniProviderImpl() throws Exception {
        this("vtdirectjni/provider.properties");
    }

    /**
     * Create the provider using a properties file
     *
     * @param propFile The properties file, must not be {@code null}
     * @throws Exception If initialization fails.
     */
    public VtdirectjniProviderImpl(final String propFile) throws Exception {
        super(propFile);
        initialize();
    }

    /**
     * Create the provider if called from the River {@link com.sun.jini.start.ServiceStarter} approach.
     *
     * @param args      Parameters that point to configuration file and possible overrides. Never {@code null}.
     * @param lifeCycle The lifecycle container to be notified if the provider terminates.
     * @throws Exception If initialization fails.
     */
    public VtdirectjniProviderImpl(final String[] args, final LifeCycle lifeCycle) throws Exception {
        super(args, lifeCycle);
        initialize();
    }

    /*
     * Provides provider specific initialization and environment checking
     */
    private void initialize() {

    }

    public Context execute(final Context context) {
        long startTime = System.currentTimeMillis();
        logger.info("\n************* Running execute *****************");
        File scratchDir = null;
        URL scratchUrl = null;
        String serviceIdString = doThreadMonitor(null);



		/*
		 * You need to fill in the details here for provider implementation.
		 */
        try {
            // cast to domain-specific context
            //
            VtdirectjniContext vtdirectjniContext = (VtdirectjniContext) context;

            // get scratch dir
            //
            scratchDir = getScratchDir(context, "vtdirectjni_execute_");
            scratchUrl = getScratchURL(scratchDir);

            // download input file
            //
            URL vtdirectInputUrl = (URL) vtdirectjniContext.getInput0();
            File vtdirectInputFile = new File(scratchDir, "direct.nml");
            URL vtdirectModelUrl = (URL) vtdirectjniContext.getInput1();
            File vtdirectModelFile = new File(scratchDir, "modelInfo.txt");
            GenericUtil.download(vtdirectInputUrl, vtdirectInputFile);
            GenericUtil.download(vtdirectModelUrl, vtdirectModelFile);

            //Process p = Runtime.getRuntime().exec("./test1");

            // create script
            //
            String vtdirectExec = null;
            if (GenericUtil.isLinux()) vtdirectExec = getProperty("provider.exec.vtdirect.linux64").trim();
            if (GenericUtil.isMac()) vtdirectExec = getProperty("provider.exec.vtdirect.mac64").trim();

            File nativeHome = new File(vtdirectExec).getParentFile();
            Vector<String> script = new Vector<String>();
            script.add("#!/bin/bash");
            script.add("cd " + scratchDir.getAbsolutePath());
            script.add("cp " + new File(vtdirectExec).getAbsolutePath() + " .");
            script.add("cp " + nativeHome + "/vtdirectObjFunc.class .");
            script.add("cp " + nativeHome + "/vtdirect .");
            //script.add("cp " + nativeHome + "/resp.txt .");
            script.add("echo \"going to run vtdirectjni exe...\" >> prvLog.txt");
            script.add("./" + new File(vtdirectExec).getName() + " &> vtdirectStdOutAndError.txt");

            File scriptFile = new File(scratchDir, "script.sh");
            GenericUtil.setFileContents(scriptFile, script);
            scriptFile.setExecutable(true);
            Process p = Runtime.getRuntime().exec(new String[]{"sh", "-c", scriptFile.getAbsolutePath()});
            p.waitFor();

            File vtdirectoutfile1 = new File(scratchDir, "directOutput.txt");
            URL vtdirectOutputFile = this.getScratchURL(vtdirectoutfile1);
            vtdirectjniContext.setOutput(vtdirectOutputFile);
            logger.info("vtdirectContext = " + vtdirectjniContext);

        } catch (Exception e) {
            StringBuilder reason = new StringBuilder();
            reason.append("*** error: vtdirectjniProviderImpl caught exception = ").append(e.getClass().getName())
                    .append("\n\tscratchDir = ").append(scratchDir)
                    .append("\n\tscratchUrl = ").append(scratchUrl);

            context.reportException(reason.toString(), e, getProviderInfo("execute"));
            logger.log(Level.SEVERE, reason.toString(), e);
            return context;
        }

        logger.info("\n\n *************** Returning from service execute ********************* \n Output context = "+context
                +"\n********************************************************************\n\n");
        return context;

    }

}
