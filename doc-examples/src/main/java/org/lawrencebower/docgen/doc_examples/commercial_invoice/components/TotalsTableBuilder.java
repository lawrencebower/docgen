package org.lawrencebower.docgen.doc_examples.commercial_invoice.components;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculationImpl;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Format;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

import static org.lawrencebower.docgen.doc_examples.commercial_invoice.CommercialInvoice.TOTAL_VALUE_NAME;

public class TotalsTableBuilder {

    private DocumentViewBuilder documentViewBuilder;
    private ComponentBuilder componentBuilder;

    public TotalsTableBuilder(DocumentViewBuilder documentViewBuilder,
                              ComponentBuilder componentBuilder) {

        this.documentViewBuilder = documentViewBuilder;
        this.componentBuilder = componentBuilder;
    }

    public TableComponent buildTotalsTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("totals table");

        tableBuilder.setWidthPercentage(100);
        tableBuilder.setTablePadding(0);

        tableBuilder.makeEmptyHeaderRowWithColWidths(70, 30);
        tableBuilder.setRenderHeader(false);

        TableComponent weightTable = makeWeightTable();

        TableComponent costTable = makeCostTable();

        tableBuilder.addRowWithComponents(weightTable, costTable);

        return tableBuilder.getTable();
    }

    private TableComponent makeWeightTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("weight table");

        tableBuilder.setWidthPercentage(100);
        tableBuilder.setRenderBorder(true);

        tableBuilder.makeEmptyHeaderRowWithColSpans(2);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("Total Number of Packages:", "1");
        tableBuilder.createRowWithLabelAndValue("Total Number of Packages:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Total Weight:", "4kg");
        tableBuilder.createRowWithLabelAndValue("Total Weight:", textComponent);
        addViewableComponent(textComponent);

        return tableBuilder.getTable();
    }

    private TableComponent makeCostTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("cost table");

        tableBuilder.setWidthPercentage(100);
        tableBuilder.setRenderBorder(true);

        tableBuilder.makeEmptyHeaderRowWithColSpans(2);

        TableTextComponent subtotalTextComponent = createTableTextComponentWithNameAndValue("Subtotal");
        TableTextComponent subTotalComponent = createTableTextComponentWithName("subTotal");
        tableBuilder.addRowWithComponents(subtotalTextComponent, subTotalComponent);

        ComponentCalculation subtotalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                         Format.CURRENCY,
                                                                         TOTAL_VALUE_NAME);
        addViewableComponent(subTotalComponent, subtotalCalc);

        TableTextComponent freightTable = createTableTextComponentWithNameAndValue("Freight");
        TableTextComponent freightComponent = createTableTextComponent("freight", "0");
        tableBuilder.addRowWithComponents(freightTable, freightComponent);

        TableTextComponent totalTextComponent = createTableTextComponentWithNameAndValue("Total");
        TableTextComponent totalComponent = createTableTextComponentWithName("total");
        tableBuilder.addRowWithComponents(totalTextComponent, totalComponent);

        ComponentCalculation totalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                      Format.CURRENCY,
                                                                      TOTAL_VALUE_NAME);
        addViewableComponent(totalComponent, totalCalc);

        TableTextComponent currencyTextComponent = createTableTextComponentWithNameAndValue("Currency Code");
        TableTextComponent currencyComponent = createTableTextComponentWithName("currency");
        tableBuilder.addRowWithComponents(currencyTextComponent, currencyComponent);

        addViewableComponent(currencyComponent);

        return tableBuilder.getTable();
    }

    private TableTextComponent createTableTextComponent(String name, String value) {
        return componentBuilder.createTableTextComponent(name, value);
    }

    private TableTextComponent createTableTextComponentWithName(String name) {
        return componentBuilder.createTableTextComponent(name);
    }

    private TableTextComponent createTableTextComponentWithNameAndValue(String name) {
        return componentBuilder.createTableTextComponent(name, name);
    }

    private void addViewableComponent(DocComponent component) {
        documentViewBuilder.addViewableComponent(component);
    }

    private void addViewableComponent(DocComponent component,
                                      ComponentCalculation calculation) {
        documentViewBuilder.addViewableComponent(component, calculation);
    }

}
