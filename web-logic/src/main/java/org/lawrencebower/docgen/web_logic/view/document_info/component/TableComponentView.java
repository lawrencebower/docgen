package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.component_calculation.table.TableComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.component_calculation.table.TableComponentCalculator;
import org.lawrencebower.docgen.web_logic.business.product_injection.TableComponentProductInjector;
import org.lawrencebower.docgen.web_logic.business.table_component.TableComponentValueSetter;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoSet;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class TableComponentView extends DocComponentView<TableComponent> {

    @Autowired
    DocComponentViewFactory viewFactory;
    @Autowired
    TableComponentValueSetter tableValueSetter;
    @Autowired
    protected TableComponentCalculator componentCalculator;

    private List<TableComponentCalculation> componentCalculations = new ArrayList<>();

    private TableComponentView() {//force spring creation
        componentViewType = ComponentViewType.TABLE;
    }

    @Override
    public void setComponent(TableComponent docComponent) {
        super.setComponent(docComponent);
    }

    @Override
    public void setComponentValue(Boolean value) {
        throw new DocGenException("Table component does not accept boolean values as value setter");
    }

    @Override
    public void setComponentValue(String value) {
        throw new DocGenException("Table component does not accept String values as value setter");
    }

    @Override
    public void setComponentValue(Float value) {
        throw new DocGenException("Table component does not accept Float values as value setter");
    }

    @Override
    public String getStringValue() {
        throw new DocGenException("Table component does not support getting value as a string");
    }

    @Override
    public Float getFloatValue() {
        throw new DocGenException("Table component does not support getting value as a boolean");
    }

    @Override
    public Boolean getBooleanValue() {
        throw new DocGenException("Table component does not support getting value as a boolean");
    }

    @Override
    public void checkAndSetValueFromParamString(String paramString, String value) {
        tableValueSetter.setCellValueIfMatch(paramString, value, this);
    }

    public int getColumnIndex(String colName) {

        int colIndex = -1;

        List<TableCell> headerCells = getHeaderCells();

        for (int i = 0; i < headerCells.size(); i++) {
            TableCell headerCell = headerCells.get(i);
            String name = headerCell.getName();
            if (colName.equals(name)) {
                colIndex = i;
            }
        }

        return colIndex;
    }

    public List<TableRow> getTableRows() {
        return docComponent.getRows();
    }

    public List<TableCell> getHeaderCells() {
        TableHeaderRow headerRow = docComponent.getHeaderRow();
        return headerRow.getCells();
    }

    public void injectProducts(List<ProductView> products) {
        TableComponentProductInjector productInjector = new TableComponentProductInjector();
        productInjector.injectProducts(docComponent, products);
    }

    public DocComponentView getCellComponentView(int rowNum, int colNum) {
        TableRow row = docComponent.getRow(rowNum);
        TableCell cell = row.getCell(colNum);
        DocComponent component = cell.getComponent();
        return viewFactory.createComponentView(component);
    }

    @Override
    public boolean hasCalculation() {
        return !componentCalculations.isEmpty();
    }

    @Override
    public void calculateValueIfNeeded(DocumentInfoSet documentSet) {
        if (hasCalculation()) {
            componentCalculator.runCalculations(this,
                                                componentCalculations,
                                                documentSet);
        }
    }


    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentInfoSet documentSet) {
        boolean operandMatched = false;

        if (operandMatched(operand)) {

            calculateValueIfNeeded(documentSet);//calculate this component if necessary

            List<Float> operandValues = getColumnValuesAsFloats(operand);

            for (Float operandValue : operandValues) {
                calculation.runOnOperand(operandValue);
            }
        }

        return operandMatched;
    }

    private boolean operandMatched(String operand) {

        boolean matched = false;

        if (hasColumnName(operand)) {
            matched = true;
        }

        return matched;
    }

    public void addComponentCalculation(TableComponentCalculation calculation) {
        componentCalculations.add(calculation);
    }

    public boolean hasColumnName(String columnName) {
        TableHeaderRow headerRow = docComponent.getHeaderRow();
        return headerRow.hasCellName(columnName);
    }

    public List<Float> getColumnValuesAsFloats(String columnName) {

        List<Float> results = new ArrayList<>();

        int columnIndex = getColumnIndex(columnName);

        List<DocComponentView> columnComponents = getComponentViewsByColumn(columnIndex);

        for (DocComponentView columnComponent : columnComponents) {
            Float value = columnComponent.getFloatValue();
            results.add(value);
        }

        return results;
    }

    private List<DocComponentView> getComponentViewsByColumn(int columnIndex) {

        List<DocComponentView> results = new ArrayList<>();

        List<TableRow> rows = getTableRows();

        for (int rowIndex = 0; rowIndex < rows.size(); rowIndex++) {
            DocComponentView view = getCellComponentView(rowIndex, columnIndex);
            results.add(view);
        }

        return results;
    }
}
