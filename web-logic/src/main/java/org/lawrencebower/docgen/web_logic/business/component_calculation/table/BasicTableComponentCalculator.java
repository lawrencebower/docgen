package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.AbstractCalculator;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView;

import java.util.ArrayList;
import java.util.List;

public class BasicTableComponentCalculator extends AbstractCalculator implements TableComponentCalculator {

    @Override
    public void runCalculation(TableComponentView tableComponentView,
                               TableComponentCalculation calculation,
                               List<DocumentInfoView> allDocs) {
        /**
         * allDocs are not used in BasicCalculator - more advanced implementation could
         * allow results to be calculated from outside the target table
         */

        String resultCol = calculation.getTargetColumn();
        int resultColIndex = getColumnIndex(resultCol, tableComponentView);

        List<String> operands = calculation.getOperands();
        List<Integer> operandColIndices = getOperandIndicies(operands, tableComponentView);

        runCalculationOnOperandsIfNeeded(operands, allDocs);

        List<TableRow> tableRows = tableComponentView.getTableRows();

        Operator operator = calculation.getOperator();

        for (int rowIndex = 0; rowIndex < tableRows.size(); rowIndex++) {
            calculateRow(rowIndex,
                         operandColIndices,
                         resultColIndex,
                         operator,
                         tableComponentView);
        }
    }

    private void calculateRow(int rowIndex,
                              List<Integer> operandColIndices,
                              int resultColIndex,
                              Operator operator,
                              TableComponentView tableComponentView) {

        List<Float> operandValues = new ArrayList<>();

        for (Integer operandIndex : operandColIndices) {
            DocComponentView docView = tableComponentView.getCellComponentView(rowIndex, operandIndex);
            Float value = docView.getFloatValue();
            operandValues.add(value);
        }

        Float calculationResult = runCalculation(operandValues, operator);

        DocComponentView resultComponent = tableComponentView.getCellComponentView(rowIndex, resultColIndex);
        resultComponent.setComponentValue(calculationResult);
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
}
