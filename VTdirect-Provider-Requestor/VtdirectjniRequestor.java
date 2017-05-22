/* The following method VtdirectRequestor.java is a requestor that creates exertions
and submits them to the grid in order to utilize services provided by the corresponding
analysis provider. */
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
package mil.afrl.mstc.open.vtdirectjni.requestor;


import mil.afrl.mstc.open.data.DataService;
import mil.afrl.mstc.open.requestor.EngineeringRequestor;
import mil.afrl.mstc.open.vtdirectjni.Vtdirectjni;
import org.sorcer.core.requestor.SorcerTester;
import sorcer.core.exertion.NetTask;
import sorcer.core.requestor.SorcerRequestor;
import sorcer.core.signature.NetSignature;
import sorcer.service.Exertion;
import sorcer.util.GenericUtil;
import sorcer.util.Sorcer;
import java.io.*;
import java.lang.*;
import java.net.URL;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;
import mil.afrl.mstc.open.vtdirectjni.VtdirectjniContext;

/**
 * A requestor that uses the following providers:
 *
 * <ul>
 * <li>@{link mil.afrl.mstc.open.vtdirectjni.Vtdirectjni}</li>
 * </ul>
 *
 * @author MSTC Engineering Project Generator
 */
public class VtdirectjniRequestor extends SorcerRequestor {
    private static Logger logger = Logger.getLogger(VtdirectjniRequestor.class.getName());
    private File dataDir = new File(System.getProperty("data.dir"));
    private DataService dataService;
    private Properties properties;
    private EngineeringRequestor engineeringRequestor = new EngineeringRequestor();

    /**
     * Run the requestor
     *
     * @param args Arguments to process
     * @throws Exception If there are problems running the requestor
     */
    public void run(String... args) throws Exception {
        //private static Logger logger = Logger.getLogger(Scott1Tester.class.getName());


        File engHome = new File(System.getProperty("eng.home"));
        File dataDir = new File(engHome, "data");
        File tempDir = new File(dataDir, "temp");

        //File vtdirectInputFile = new File(engHome,"/optimization/vtdirectjni/vtdirectjni-req/data/direct.nml");
        File vtdirectInputFile = new File(engHome,"/optimization/vtdirectjni/vtdirectjni-req/data/direct.nml");
        //File modelInputFile = new File(engHome,"/optimization/vtdirectjni/vtdirectjni-req/data/modelInfo.txt");
        File modelInputFile = new File(engHome,"/optimization/vtdirectjni/vtdirectjni-req/data/modelInfo.txt");

        // edit modeInputFile to adjust model name for user
        Vector<String> modelInputFileContents = GenericUtil.getFileContents(modelInputFile);
        GenericUtil.printVect(modelInputFileContents);
        modelInputFileContents.setElementAt(Sorcer.getActualName(modelInputFileContents.elementAt(0).trim()), 0);
        GenericUtil.printVect(modelInputFileContents);

        File modelInputFileTemp = new File(tempDir, "modelInfo.txt");
        GenericUtil.setFileContents(modelInputFileTemp, modelInputFileContents);


        URL vtdirectInputUrl = SorcerTester.copyFileToScratchAndGetUrl(vtdirectInputFile, dataDir);
        URL modelInputUrl = SorcerTester.copyFileToScratchAndGetUrl(modelInputFileTemp, dataDir);

        VtdirectjniContext context = new VtdirectjniContext("VTdirectJNIContext");
        context.setInput0(vtdirectInputUrl);
        context.setInput1(modelInputUrl);

        /* construct execute method and then Task */
        String providerName = Sorcer.getActualName("Engineering-Vtdirectjni");
        String serviceName = "execute";
        NetSignature methodEN = new NetSignature(serviceName,
                    Vtdirectjni.class,
                    providerName);

        NetTask vtdirectTask = new NetTask("run execute", "Task to run VTDIRECT95", methodEN);

        /* Put the context into the task */
        vtdirectTask.setContext(context);
        logger.info("scott1Task = " + vtdirectTask);
        Exertion result = vtdirectTask.exert();
        logger.info("Returned Task after exert: " + result);
        logger.info("Context output = "+((VtdirectjniContext)result.getContext()).getOutput());

        URL vtdirectOutputUrl = (URL) ((VtdirectjniContext) result.getContext()).getOutput();
        Vector<String> vtdirectOutputFileContents = GenericUtil.getFileContents(vtdirectOutputUrl);
        GenericUtil.printVect(vtdirectOutputFileContents);

        }
    }


    
