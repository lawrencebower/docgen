package org.lawrencebower.docgen.web.test_examples.test_doc_1.components;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableBuilder;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableComponent;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Format;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_logic.business.component_calculation.table.TableComponentCalculationImpl;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

import java.awt.*;

public class Doc1PackageInformationTableBuilder {

    private DocumentViewBuilder documentViewBuilder;

    public TableComponent buildTablePackageInformationTable(DocumentViewBuilder documentViewBuilder) {

        this.documentViewBuilder = documentViewBuilder;

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("package information");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell("Package Information");
        shippedFromCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerRow.addCell(shippedFromCell);
        tableBuilder.setHeaderRow(headerRow);

        TableComponent productTable = makeProductTable();

        tableBuilder.addRowWithComponents(productTable);

        tableBuilder.setWidthPercentage(100);
        tableBuilder.setRenderBorder(true);
        tableBuilder.setTablePadding(0);

        return tableBuilder.getTable();
    }

    private TableComponent makeProductTable() {

        ViewTableBuilder tableBuilder = new ViewTableBuilder("product table");

        ViewHeaderRow headerRow = new ViewHeaderRow();

        String quantityDisplayName = "Number of Units";
        String quantityName = ProductInjectionField.PRODUCT_QUANTITY.getName();
        ViewHeaderCell quantityCell = new ViewHeaderCell(quantityName);
        quantityCell.setText(quantityDisplayName);
        headerRow.addCell(quantityCell);

        String productNameName = ProductInjectionField.PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION.getName();
        ViewHeaderCell nameCell = new ViewHeaderCell(productNameName);
        nameCell.setText("Description");
        headerRow.addCell(nameCell);

        String productValueName = ProductInjectionField.PRODUCT_VALUE.getName();
        ViewHeaderCell valueCell = new ViewHeaderCell(productValueName);
        valueCell.setText("Unit Value");
        headerRow.addCell(valueCell);

        String productOriginName = ProductInjectionField.PRODUCT_ORIGIN.getName();
        ViewHeaderCell originCell = new ViewHeaderCell(productOriginName);
        originCell.setText("Country of origin");
        headerRow.addCell(originCell);

        String TOTAL_VALUE_NAME = "totalValue";

        ViewHeaderCell totalCell = new ViewHeaderCell(TOTAL_VALUE_NAME);
        totalCell.setText("Total Value");
        headerRow.addCell(totalCell);

        tableBuilder.setHeaderRow(headerRow);

        tableBuilder.setWidthPercentage(100);
        tableBuilder.setRenderBorder(true);

        TableComponentCalculationImpl calculation = new TableComponentCalculationImpl(Operator.MULTIPLY,
                                                                                      Format.CURRENCY,
                                                                                      TOTAL_VALUE_NAME,
                                                                                      quantityName,
                                                                                      productValueName);
        ViewTableComponent table = tableBuilder.getTable();

        addViewableComponent(table, calculation);

        return table;

    }

    private void addViewableComponent(DocComponent component,
                                      TableComponentCalculationImpl calculation) {

        documentViewBuilder.addViewableComponent(component, calculation);
    }
}
