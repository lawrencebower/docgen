package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.table.TableComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.table.TableComponentCalculator;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;

import java.util.ArrayList;
import java.util.List;

public class BasicTableComponentCalculator implements TableComponentCalculator {

    private TableComponentView tableComponentView;
    private List<TableComponentCalculation> calculations;

    @Override
    public void runCalculations(TableComponentView tableComponentView,
                                List<TableComponentCalculation> calculations,
                                DocumentSet documentSet) {

        this.tableComponentView = tableComponentView;
        this.calculations = calculations;

        for (TableComponentCalculation calculation : calculations) {
            runCalculationIfNeeded(documentSet, calculation);
        }

    }

    private void runCalculationIfNeeded(DocumentSet documentSet,
                                        TableComponentCalculation calculation) {
        if (calculation.isNotRun()) {
            calculation.clearResult();
            runCalculation(calculation, documentSet);
        }
    }

    private void runCalculation(TableComponentCalculation calculation,
                                DocumentSet documentSet) {

        String resultCol = calculation.getTargetColumn();
        int resultColIndex = getColumnIndex(resultCol, tableComponentView);

        List<String> operands = calculation.getOperands();
        List<Integer> operandColIndices = getOperandIndicies(operands, tableComponentView);

        runCalculationOnOperandsIfNeeded(operands, documentSet);

        List tableRows = tableComponentView.getTableRows();

        for (int rowIndex = 0; rowIndex < tableRows.size(); rowIndex++) {

            runCalculationOnRow(rowIndex,
                                operandColIndices,
                                calculation);

            DocComponentView resultComponent = tableComponentView.getCellComponentView(rowIndex, resultColIndex);
            String calculationResult = calculation.getFormattedResult();
            resultComponent.setComponentValue(calculationResult);
        }
    }

    private void runCalculationOnOperandsIfNeeded(List<String> operands,
                                                  DocumentSet documentSet) {
        for (String operand : operands) {
            if (operandMatchesCalculation(operand)) {
                TableComponentCalculation calculation = getCalculationMatchingOperand(operand);
                runCalculationIfNeeded(documentSet, calculation);
            }
        }

    }

    private boolean operandMatchesCalculation(String operand) {

        boolean operandMatched = false;

        for (TableComponentCalculation calculation : calculations) {
            String targetColumn = calculation.getTargetColumn();
            if (operand.equals(targetColumn)) {
                operandMatched = true;
            }
        }

        return operandMatched;
    }

    private TableComponentCalculation getCalculationMatchingOperand(String operand) {

        for (TableComponentCalculation calculation : calculations) {
            String targetColumn = calculation.getTargetColumn();
            if (operand.equals(targetColumn)) {
                return calculation;
            }
        }

        String message = String.format("No calculation found matching targetColumn '%s'", operand);
        throw new DocGenException(message);
    }

    private void runCalculationOnRow(int rowIndex,
                                     List<Integer> operandColIndices,
                                     TableComponentCalculation calculation) {

        List<String> operandValues = new ArrayList<>();

        for (Integer operandIndex : operandColIndices) {
            DocComponentView componentView = tableComponentView.getCellComponentView(rowIndex, operandIndex);
            String value = componentView.getStringValue();
            operandValues.add(value);
        }

        runCalculation(operandValues, calculation);

    }

    private List<Integer> getOperandIndicies(List<String> operands,
                                             TableComponentView tableComponentView) {

        List<Integer> results = new ArrayList<>();

        for (String operand : operands) {

            int index = getColumnIndex(operand, tableComponentView);
            results.add(index);
        }

        return results;
    }

    private int getColumnIndex(String colName, TableComponentView tableComponentView) {

        int columnIndex = tableComponentView.getColumnIndex(colName);

        if (columnIndex == -1) {
            String message = "BasicTableComponentCalculator does not support calculations with operand components " +
                             "outside the target table";
            throw new DocGenException(message);
        }

        return columnIndex;
    }

    private void runCalculation(List<String> operandValues, TableComponentCalculation calculation) {

        checkOperandSize(operandValues);
        calculation.clearResult();

        for (String operandValue : operandValues) {
            calculation.runOnOperand(operandValue);
        }

    }

    private void checkOperandSize(List<String> operandValues) {
        if ((operandValues == null) || operandValues.isEmpty()) {
            throw new DocGenException("List of operands is empty!?");
        }
    }
}
