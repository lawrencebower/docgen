package org.lawrencebower.docgen.web_model.view.document.component;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.table.TableComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.table.TableComponentCalculator;
import org.lawrencebower.docgen.web_model.business_def.injection.TableComponentProductInjector;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.table_component.TableComponentValueSetter;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class TableComponentView extends DocComponentViewImpl<TableComponent> {

    @Autowired
    DocComponentViewFactory viewFactory;
    @Autowired(required = false)
    TableComponentValueSetter tableValueSetter;
    @Autowired(required = false)
    protected TableComponentCalculator componentCalculator;
    @Autowired(required = false)
    private TableComponentProductInjector productInjector;

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
    public String getStringValue() {
        throw new DocGenException("Table component does not support getting value as a string");
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
        productInjector.injectProducts(docComponent, products);
    }

    @Override
    public void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo) {
        //not implemented - just exit quietly
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
    public void calculateValueIfNeeded(DocumentSet documentSet) {
        if (hasCalculation()) {
            componentCalculator.runCalculations(this,
                                                componentCalculations,
                                                documentSet);
        }
    }

    @Override
    public void setComponentCalculation(ComponentCalculation calculation) {

        if (!(calculation instanceof TableComponentCalculation)) {
            throw new DocGenException("TableComponentView only accepts TableComponentCalculations");
        }

        componentCalculations.clear();
        componentCalculations.add((TableComponentCalculation) calculation);
    }

    public void addComponentCalculation(TableComponentCalculation calculation) {
        componentCalculations.add(calculation);
    }

    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentSet documentSet) {
        boolean operandMatched = false;

        if (operandMatched(operand)) {

            calculateValueIfNeeded(documentSet);//calculate this component if necessary

            List<String> operandValues = getColumnValuesAsFloats(operand);

            for (String operandValue : operandValues) {
                calculation.runOnOperand(operandValue);
            }
        }

        return operandMatched;
    }

    @Override
    public void copyFromDocument(DocumentView documentToCopy) {
        /**
         * not needed yet - if we need to copy a documentToCopy with a product table this will need to be implemented
         */
        //todo
    }

    private boolean operandMatched(String operand) {

        boolean matched = false;

        if (hasColumnName(operand)) {
            matched = true;
        }

        return matched;
    }

    public boolean hasColumnName(String columnName) {
        TableHeaderRow headerRow = docComponent.getHeaderRow();
        return headerRow.hasCellName(columnName);
    }

    public List<String> getColumnValuesAsFloats(String columnName) {

        List<String> results = new ArrayList<>();

        int columnIndex = getColumnIndex(columnName);

        List<DocComponentView> columnComponents = getComponentViewsByColumn(columnIndex);

        for (DocComponentView columnComponent : columnComponents) {
            String value = columnComponent.getStringValue();
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
