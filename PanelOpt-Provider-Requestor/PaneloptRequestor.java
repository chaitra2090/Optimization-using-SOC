/* The following method PaneloptRequestor.java is a requestor that creates exertions
and submits them to the grid in order to utilize services provided by the corresponding
analysis provider.
*/

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
package mil.afrl.mstc.open.panelopt.requestor;

import mil.afrl.mstc.open.panelopt.Panelopt;
import mil.afrl.mstc.open.panelopt.PaneloptContext;

import mil.afrl.mstc.open.data.DataService;
import mil.afrl.mstc.open.requestor.EngineeringRequestor;
import org.sorcer.core.requestor.SorcerTester;
import sorcer.core.exertion.NetTask;
import sorcer.core.requestor.SorcerRequestor;
import sorcer.core.signature.NetSignature;
import sorcer.service.*;
import sorcer.util.GenericUtil;
import sorcer.util.Sorcer;
import java.io.*;
import java.lang.*;
import java.net.URL;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;


/**
 * A requestor that uses the follopanel providers:
 * <p/>
 * <ul>
 * <li>@{link mil.afrl.mstc.open.panelopt.Panelopt}</li>
 * </ul>
 *
 * @author MSTC Engineering Project Generator
 */
public class PaneloptRequestor extends SorcerRequestor {
    private static Logger logger = Logger.getLogger(PaneloptRequestor.class.getName());
    private File dataDir = new File(System.getProperty("data.dir"));
    private DataService dataService;
    private Properties properties;
    private EngineeringRequestor engineeringRequestor = new EngineeringRequestor();

    public void runTest(boolean useSpace) throws Exception {

        long startTime = System.currentTimeMillis();
        Task panelOptTask = getTask();

        panelOptTask.getControlContext().setAccessType(Strategy.Access.PUSH);
        if (useSpace) panelOptTask.getControlContext().setAccessType(Strategy.Access.PULL);

        Exertion result = panelOptTask.exert();
        PaneloptContext ctx = (PaneloptContext) result.getContext();

        System.out.println("\nuseSpace = " + useSpace);
        System.out.println("requestor call elapsed time [s] = " + (System.currentTimeMillis() - startTime) / 1000.0);
        System.out.println("system call elapsed time [s] = " + ctx.sysCallElapsedTimeMilliSeconds / 1000.0);
        System.out.println("service op call elapsed time [s] = " + ctx.serviceOpElapsedTimeMilliSeconds / 1000.0);
        System.out.println("\n");
    }
    
	/*
	Run the requestor.
	@param args Arguments to process. @throws Exception if there are
	problems running the requestor.
	*/
	public void run(String... args) throws Exception {
        File engHome = new File(System.getProperty("eng.home"));
        File dataDir = new File(engHome, "data");
        File tempDir = new File(dataDir, "temp");
	
		// Get input files required by the EBF3PanelOpt framework
        File panelOptInputFile = new File(engHome, "/products/panelopt/panelopt-req/data/stiffened_plate_input_data.txt");
        File panelOptDesVarsInputFile = new File(engHome, "/products/panelopt/panelopt-req/data/dvar.vef");

		//Copy input files to the scratch directory
        URL panelOptInputUrl = SorcerTester.copyFileToScratchAndGetUrl(panelOptInputFile, dataDir);
        URL panelOptDesVarsInputUrl = SorcerTester.copyFileToScratchAndGetUrl(panelOptDesVarsInputFile, dataDir);

		// Add input to Context
        PaneloptContext context = new PaneloptContext("PaneloptContext");
		context.setPanelOptInput(panelOptInputUrl);
        Vector<String> desVarStrings = GenericUtil.getFileContents(panelOptDesVarsInputUrl);
        Double[] dvArray = new Double[desVarStrings.size()];
        int i = 0;
        for (String dvs: desVarStrings) {
            dvArray[i++] = new Double(dvs);
        }
        context.dvArray = dvArray;

        // Construct execute method and then Task
        String providerName = Sorcer.getActualName("Engineering-Panelopt");
        String serviceName = "execute";
        NetSignature methodEN = new NetSignature(serviceName,
                Panelopt.class,
                providerName);
		NetTask panelOptTask = new NetTask("run execute", "Task to run panel opt", methodEN);
        panelOptTask.setContext(context);
        
        //Put the context into the task
		logger.info("panelOptTask = " + panelOptTask);
		Exertion result = panelOptTask.exert();
		logger.info("Returned Task after exert: " + result);
		logger.info("outputArray = "+
		GenericUtil.arrayToString(((PaneloptContext)result.getContext()).outputArray));
		System.out.println("dvArray = "+
		GenericUtil.arrayToString(((PaneloptContext)result.getContext()).dvArray));
		System.out.println("outputArray = " +
		GenericUtil.arrayToString(((PaneloptContext)
		result.getContext()).outputArray));
    }
}


    
