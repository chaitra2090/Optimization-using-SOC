/* The following method contains the dependent and independent vars that define a specific optimization problem. 
The fundamentals of a model provider are discussed in Chapter 3.3. */
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
package mil.afrl.mstc.products.vtex1;

import mil.afrl.mstc.open.panelopt.PaneloptContext;
import mil.afrl.mstc.open.panelopt.provider.PaneloptProviderImpl;
import org.sorcer.core.requestor.SorcerTester;
import sorcer.core.context.model.PoolStrategy;
import sorcer.core.exertion.NetTask;
import sorcer.core.signature.NetSignature;
import sorcer.core.signature.ObjectSignature;
import sorcer.modeling.core.context.model.opti.OptimizationModel;
import sorcer.modeling.vfe.Filter;
import sorcer.modeling.vfe.Var;
import sorcer.modeling.vfe.evaluator.ExertionEvaluator;
import sorcer.modeling.vfe.evaluator.MethodEvaluator;
import sorcer.modeling.vfe.filter.*;
import sorcer.modeling.vfe.util.VarList;
import sorcer.service.*;
import sorcer.service.modeling.Variability;
import sorcer.util.Sorcer;
import sorcer.service.Strategy.Access;

import java.io.File;
import java.net.URL;
import java.util.Properties;
import java.util.logging.Logger;

import static sorcer.eo.operator.sig;
import static sorcer.modeling.vo.operator.task;

public class PanelOptModel {
	
	protected static final Logger logger = Logger.getLogger(PanelOptModel.class.getName());
	public static Properties props;
	public static File dataDir = new File(System.getProperty("data.dir"));
	public static File engHome = new File(System.getProperty("eng.home"));

	static {
		if (props == null) {
			props = Sorcer.loadPropertiesNoException(System.getProperty("provider.properties", "provider.properties"));
		}
	}

	public static OptimizationModel getModel() {
	
		OptimizationModel model = null;

		try {
			// Initialize the design variables
			Double[] dvArray = {0.0, 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18, 19.19,20.20, 21.21, 22.22, 23.23, 24.24};
			VarList designVars = new VarList();

			// Initialize the individual vars
			Var<Double> x1 = getDesignVar(0, dvArray, designVars);
			Var<Double> x2 = getDesignVar(1, dvArray , designVars);
			Var<Double> x3 = getDesignVar(2, dvArray , designVars);
			Var<Double> x4 = getDesignVar(3, dvArray , designVars);
			Var<Double> x5 = getDesignVar(4, dvArray , designVars);
			Var<Double> x6 = getDesignVar(5, dvArray , designVars);
			Var<Double> x7 = getDesignVar(6, dvArray , designVars);
			Var<Double> x8 = getDesignVar(7, dvArray , designVars);
			Var<Double> x9 = getDesignVar(8, dvArray , designVars);
			Var<Double> x10 = getDesignVar(9, dvArray , designVars);
			Var<Double> x11 = getDesignVar(10, dvArray , designVars);
			Var<Double> x12 = getDesignVar(11, dvArray , designVars);
			Var<Double> x13 = getDesignVar(12, dvArray , designVars);
			Var<Double> x14 = getDesignVar(13, dvArray , designVars);
			Var<Double> x15 = getDesignVar(14, dvArray , designVars);
			Var<Double> x16 = getDesignVar(15, dvArray , designVars);
			Var<Double> x17 = getDesignVar(16, dvArray , designVars);
			Var<Double> x18 = getDesignVar(17, dvArray , designVars);
			Var<Double> x19 = getDesignVar(18, dvArray , designVars);
			Var<Double> x20 = getDesignVar(19, dvArray , designVars);
			Var<Double> x21 = getDesignVar(20, dvArray , designVars);
			Var<Double> x22 = getDesignVar(21, dvArray , designVars);
			Var<Double> x23 = getDesignVar(22, dvArray , designVars);
			Var<Double> x24 = getDesignVar(23, dvArray , designVars);
			Var<Double> x25 = getDesignVar(24, dvArray , designVars);  

			File dataDir = new File(System.getProperty("data.dir"));
			File engHome = new File(System.getProperty("eng.home"));

			// Add input to Context
			PaneloptContext ctx = new PaneloptContext();
			ctx.dvArray = dvArray;
	
			// Option to use Space or Catalog
			boolean useSpace = true;
			boolean useIntra = false;
			
			// Invocation to ExertEvaluator
			ExertionEvaluator yEval = getExertionEvaluator(ctx, useSpace, useIntra);
			yEval.addArgs(designVars);

			// Initialize response variables
			VarList dependentVars = new VarList();
			Var<Double> y1 = getOutputVar(0, yEval, dependentVars);
			Var<Double> y2 = getOutputVar(1, yEval, dependentVars);
			Var<Double> y3 = getOutputVar(2, yEval, dependentVars);
			Var<Double> y4 = getOutputVar(3, yEval, dependentVars);
			Var<Double> y5 = getOutputVar(4, yEval, dependentVars);
	
			// Construct model with dependent and independent vars
			model = constructModel("PanelOptModel", designVars, dependentVars, new VarList(), new VarList());

			/* The information below is used when a table is passed to the model;  if
			   no explicit strategy+builder is passed in the context that executes a table,
			   the builder is pulled from the strategy which was published with the model
			   which is done below. */

			PoolStrategy strategy = new PoolStrategy();
			strategy.setBuilder(new ObjectSignature("getModel", PanelOptModel.class));
			model.setStrategy(strategy);

		} catch (Exception e) {
			logger.severe(e.toString());
			e.printStackTrace();
		}

		return model;
	}

	private static OptimizationModel constructModel(String modelName
			, VarList designVarList, VarList dependentVarList
			, VarList objVarList, VarList conVarList) throws ContextException {
		for (Var<?> v : designVarList) {
			v.setType(Variability.Type.INPUT);
			v.addKind(Variability.Type.DESIGN);
		}
		for (Var<?> v : dependentVarList) {
			v.setType(Variability.Type.OUTPUT);
		}

		// Construct model and add vars
		OptimizationModel omodel = new OptimizationModel(modelName);

		// Place holders for null pointer error
		omodel.putConstantVars(new VarList());
		omodel.putLinkedVars(new VarList());
		omodel.putConstraintVars(new VarList());
		omodel.putWatchableVars(new VarList());
		omodel.putObjectiveVars(new VarList());
		omodel.putInvariantVars(new VarList());
		omodel.putInputVars(designVarList);
		omodel.putOutputVars(dependentVarList);
		omodel.putObjectiveVars(objVarList);
		omodel.putConstraintVars(conVarList);

		return omodel;
	}

	protected static ExertionEvaluator getExertionEvaluator(Context<?> context
															, boolean useSpace
															, boolean useIntra) throws SignatureException {
		Task task = null;
		String serviceOp = "execute";
		if (useIntra) {
			task = task(sig(serviceOp, PaneloptProviderImpl.class), context);
		} else {
			NetSignature method = new NetSignature(serviceOp, mil.afrl.mstc.open.panelopt.Panelopt.class, Sorcer.getActualName("Engineering-Panelopt"));
			task = new NetTask("Task name: execute", method);
			task.setContext(context);
			task.getControlContext().setAccessType(Strategy.Access.PUSH); // catalog
			if (useSpace) task.getControlContext().setAccessType(Strategy.Access.PULL); // space
		}
		return new ExertionEvaluator(task);
	}



	private static Var<Double> getDesignVar(int index, Double[] dvArray, VarList designVars) {
		Var<Double> var = new Var("x" + index, dvArray[index]);
		var.setSetter(new ArraySetter(dvArray, index));
		designVars.add(var);
		return var;
	}

	private static Var<Double> getOutputVar(int index, ExertionEvaluator eval, VarList dependentVars) {
		Filter contextGetter = new ContextGetter();
		Filter outputFilter = new ObjectFieldFilter("outputArray");
		Var<Double> y = new Var<Double>("y" + index);
		y.setEvaluator(eval);
		y.setFilter(new Filter(contextGetter, outputFilter, new ArrayFilter(index)));
		dependentVars.add(y);
		return y;
	}
}

