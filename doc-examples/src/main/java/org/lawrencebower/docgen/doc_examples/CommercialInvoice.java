package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.generator.custom.CustomDocumentInfo;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.custom.component.*;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.product_injection.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;

public class CommercialInvoice {

    @Autowired
    private CustomPDFGenerator pdfGenerator;
    @Autowired
    private CustomComponentFactory componentFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;

    private CustomDocumentInfo docInfo;
    private DocumentInfoView docInfoView;

    public static final String INVOICE_NAME = "invoice";

    public void prepareComponents() {

        docInfo = new CustomDocumentInfo(INVOICE_NAME, pdfGenerator);

        docInfoView = new DocumentInfoView(docInfo);

        TableComponent addressTable = makeInvoiceTable();

        TableComponent packageInformationTable = makePackageInformationTable();

        docInfo.addComponent(convertComponent(addressTable));

        docInfo.addComponent(convertComponent(new NewLineComponent()));

        docInfo.addComponent(convertComponent(packageInformationTable));

        TableComponent totalsTable = makeTotalsTable();

        docInfo.addComponent(convertComponent(totalsTable));

        docInfo.addComponent(convertComponent(new NewLineComponent()));

        TextComponent signature = new TextComponent("David Davidson");
        signature.setName("signature");

        docInfo.addComponent(convertComponent(signature));

        docInfo.addComponent(convertComponent(new LineComponent(70)));
        docInfo.addComponent(convertComponent(new TextComponent("Signature of exporter (print and sign)\n" +
                                                                "I declare all the information to be accurate and correct")));

        docInfo.addComponent(convertComponent(new NewLineComponent()));

        TextComponent date = new TextComponent("29th May 2012");
        date.setName("Date:");
        docInfo.addComponent(convertComponent(date));
        addTextComponentView(date);

        docInfo.addComponent(convertComponent(new LineComponent(30)));
        docInfo.addComponent(convertComponent(new TextComponent("Date")));

    }

    private CustomComponent convertComponent(DocComponent component) {
        return componentFactory.createCustomComponent(component);
    }

    private TableComponent makeInvoiceTable() {

        TableComponent invoiceTable = new TableComponent("invoice table");
        invoiceTable.setTablePadding(0);

        TableHeaderRow headerRow = new TableHeaderRow();

        TableTextComponent headerComponent = new TableTextComponent(HorizontalAlignment.CENTER, "Commercial Invoice");
        TableCell headerCell = new TableCell(headerComponent);
        headerCell.setPadding(3);
        headerCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerCell.setColSpan(2);
        headerRow.addCell(headerCell);

        invoiceTable.setHeaderRow(headerRow);

        TableComponent shippedFrom1Table = makeShippedFrom1Table();
        TableComponent shippedFrom2Table = makeShippedFrom2Table();
        TableComponent shippedToTable = makeShippedToTable();
        TableComponent soldToTable = makeSoldToTable();

        TableRow row1 = new TableRow();
        row1.addCell(new TableCell(shippedFrom1Table));
        row1.addCell(new TableCell(shippedFrom2Table));

        invoiceTable.addRow(row1);

        TableRow row2 = new TableRow();
        row2.addCell(new TableCell(shippedToTable));
        row2.addCell(new TableCell(soldToTable));

        invoiceTable.addRow(row2);

        invoiceTable.setWidthPercentage(100);

        invoiceTable.setRenderBorder(true);

        return invoiceTable;
    }

    private TableComponent makeSoldToTable() {

        TableComponent table = new TableComponent("sold to");

        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();
        TableCell shippedToCell = new TableCell("SOLD TO");
        shippedToCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedToCell.setColSpan(2);
        headerRow.addCell(shippedToCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("soldToName:", "blah");
        table.addRow(createRowWithLabelAndValue("Name:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.BUSINESS_NAME);

        textComponent = createTextComponent("soldToContactName", "David Davidson");
        table.addRow(createRowWithLabelAndValue("Contact Name:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.BUSINESS_CONTACT_NAME);

        textComponent = createTextComponent("soldToPhone", "123456788");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.BUSINESS_PHONE);

        textComponent = createTextComponent("soldToCompanyAddress",
                                            "Suites 11 & 12\n" +
                                            "Church Farm,\n" +
                                            "Maris Lane,\n" +
                                            "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.BUSINESS_ADDRESS);

        textComponent = createTextComponent("soldToCountry", "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.BUSINESS_COUNTRY);

        return table;
    }

    private TableComponent makeShippedToTable() {

        TableComponent table = new TableComponent("shipped to");

        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();
        TableCell shippedToCell = new TableCell("SHIPPED TO");
        shippedToCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedToCell.setColSpan(2);
        headerRow.addCell(shippedToCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("customerName", "");
        table.addRow(createRowWithLabelAndValue("Name:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.CUSTOMER_NAME);

        textComponent = createTextComponent("customerContact", "Billy Bob Bobson");
        table.addRow(createRowWithLabelAndValue("Contact Name:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.CUSTOMER_CONTACT_NAME);

        textComponent = createTextComponent("Contact phone", "123456788");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.CUSTOMER_PHONE);

        textComponent = createTextComponent("customerAddress",
                                            "Suites 11 & 12\n" +
                                            "Church Farm,\n" +
                                            "Maris Lane,\n" +
                                            "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addTextAreaComponent(textComponent,
                             AutoMappedComponent.CUSTOMER_ADDRESS);

        textComponent = createTextComponent("customerCountry", "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.CUSTOMER_COUNTRY);

        return table;
    }

    private TableComponent makeShippedFrom2Table() {

        TableComponent table = new TableComponent("shipped from 2");

        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();
        TableCell shippedFromCell = new TableCell("");
        shippedFromCell.setColSpan(2);
        headerRow.addCell(shippedFromCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("Date:", "29th May 2012");
        table.addRow(createRowWithLabelAndValue("Date:", textComponent));
        addTextComponentView(textComponent);

        textComponent = createTextComponent("Reference/Order No:", "154487945");
        table.addRow(createRowWithLabelAndValue("Reference/Order No:", textComponent));
        addTextComponentView(textComponent);

        textComponent = createTextComponent("Airbill Number:", "45678945");
        table.addRow(createRowWithLabelAndValue("Airbill Number:", textComponent));
        addTextComponentView(textComponent);

        textComponent = createTextComponent("Reason For Export:", "SOLD");
        table.addRow(createRowWithLabelAndValue("Reason For Export:", textComponent));

        textComponent = createTextComponent("Incoterms:", "FOB");
        table.addRow(createRowWithLabelAndValue("Incoterms:", textComponent));

        return table;
    }

    private void addTextComponentView(TextComponent textComponent) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextComponentView(TableTextComponent textComponent) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextComponent(TableTextComponent textComponent, AutoMappedComponent autoMappedComponent) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        componentView.setAutoMappedComponent(autoMappedComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextAreaComponent(TableTextComponent textComponent,
                                      AutoMappedComponent autoMappedComponent) {
        DocComponentView componentView = componentViewFactory.createTextAreaComponentView(textComponent);
        componentView.setAutoMappedComponent(autoMappedComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTableComponentView(TableComponent tableComponent) {
        TableComponentView componentView = componentViewFactory.createTableComponentView(tableComponent);
        docInfoView.addComponentView(componentView);
    }

    private TableComponent makeShippedFrom1Table() {
        TableComponent table = new TableComponent("shipped from");

        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();
        TableCell shippedFromCell = new TableCell("SHIPPED FROM");
        shippedFromCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedFromCell.setColSpan(2);
        headerRow.addCell(shippedFromCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("shippedFromName", "Acme Ltd");
        table.addRow(createRowWithLabelAndValue("Name:", textComponent));
        addTextComponent(textComponent, AutoMappedComponent.VENDOR_NAME);

        textComponent = createTextComponent("TAX/VAT NUMBER:", "154487945");
        table.addRow(createRowWithLabelAndValue("TAX/VAT NUMBER:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.VENDOR_TAX_ID);

        textComponent = createTextComponent("shippedFromContactName", "David Davidson");
        table.addRow(createRowWithLabelAndValue("Contact Name:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.VENDOR_CONTACT_NAME);

        textComponent = createTextComponent("shippedFromPhone", "+44 (0)1223 655577");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.VENDOR_PHONE);

        textComponent = createTextComponent("shippedFromTable",
                                            "Suites 11 & 12\n" +
                                            "Church Farm,\n" +
                                            "Maris Lane,\n" +
                                            "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addTextAreaComponent(textComponent,
                             AutoMappedComponent.VENDOR_ADDRESS);

        textComponent = createTextComponent("shippedFromCountry", "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addTextComponent(textComponent,
                         AutoMappedComponent.VENDOR_COUNTRY);


        return table;
    }

    private TableComponent makePackageInformationTable() {
        TableComponent table = new TableComponent("package information");

        TableHeaderRow headerRow = new TableHeaderRow();
        TableCell shippedFromCell = new TableCell("Package Information");
        shippedFromCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerRow.addCell(shippedFromCell);
        table.setHeaderRow(headerRow);

        TableComponent productTable = makeProductTable();

        TableRow row = new TableRow();
        row.addCell(new TableCell(productTable));
        table.addRow(row);

        table.setWidthPercentage(100);
        table.setRenderBorder(true);
        table.setTablePadding(0);

        return table;
    }

    private TableComponent makeProductTable() {

        TableComponent productTable = new TableComponent("product table");

        TableHeaderRow headerRow = new TableHeaderRow();

        TextComponent quantityComponent = new TextComponent("Number of Units");
        quantityComponent.setName(ProductInjectionField.PRODUCT_QUANTITY.getName());
        TableCell quantityCell = new TableCell(quantityComponent);
        headerRow.addCell(quantityCell);

        TextComponent nameComponent = new TextComponent("Description");
        nameComponent.setName(ProductInjectionField.PRODUCT_NAME.getName());
        TableCell nameCell = new TableCell(nameComponent);
        headerRow.addCell(nameCell);

        TextComponent valueComponent = new TextComponent("Unit Value");
        valueComponent.setName(ProductInjectionField.PRODUCT_VALUE.getName());
        TableCell valueCell = new TableCell(valueComponent);
        headerRow.addCell(valueCell);

        TextComponent originComponent = new TextComponent("Country of origin");
        originComponent.setName(ProductInjectionField.PRODUCT_ORIGIN.getName());
        TableCell originCell = new TableCell(originComponent);
        headerRow.addCell(originCell);

        headerRow.addCell(new TableCell("Total Value"));

        productTable.setHeaderRow(headerRow);

        productTable.setWidthPercentage(100);
        productTable.setRenderBorder(true);

        addTableComponentView(productTable);
        return productTable;
    }

    private TableComponent makeTotalsTable() {

        TableComponent totalsTable = new TableComponent("totals table");

        totalsTable.setWidthPercentage(100);
        totalsTable.setTablePadding(0);

        TableHeaderRow headerRow = new TableHeaderRow();
        headerRow.setRenderHeader(false);
        TableCell headerCell = new TableCell();
        headerCell.setColSpan(2);
        headerRow.addCell(headerCell);
        headerRow.setColumnWidths(70, 30);
        totalsTable.setHeaderRow(headerRow);

        TableRow contentsRow = new TableRow();

        TableComponent weightTable = makeWeightTable();

        TableComponent costTable = makeCostTable();

        contentsRow.addCell(new TableCell(weightTable));
        contentsRow.addCell(new TableCell(costTable));

        totalsTable.addRow(contentsRow);

        return totalsTable;
    }

    private TableComponent makeWeightTable() {

        TableComponent weightTable = new TableComponent("weight table");

        weightTable.setWidthPercentage(100);
        weightTable.setRenderBorder(true);

        TableHeaderRow headerRow = new TableHeaderRow();
        headerRow.setRenderHeader(false);
        TableCell cell = new TableCell();
        cell.setColSpan(2);
        headerRow.addCell(cell);
        weightTable.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("Total Number of Packages:", "1");
        weightTable.addRow(createRowWithLabelAndValue("Total Number of Packages:", textComponent));

        textComponent = createTextComponent("Total Weight:", "4kg");
        weightTable.addRow(createRowWithLabelAndValue("Total Weight:", textComponent));

        return weightTable;
    }

    private TableComponent makeCostTable() {

        TableComponent costTable = new TableComponent("cost table");

        costTable.setWidthPercentage(100);
        costTable.setRenderBorder(true);

        TableHeaderRow headerRow = new TableHeaderRow();
        headerRow.setRenderHeader(false);
        TableCell cell = new TableCell();
        cell.setColSpan(2);
        headerRow.addCell(cell);
        costTable.setHeaderRow(headerRow);

        TableTextComponent textComponent = createTextComponent("Subtotal", "10");
        costTable.addRow(createRowWithLabelAndValue("Subtotal", textComponent));

        textComponent = createTextComponent("Freight", "12");
        costTable.addRow(createRowWithLabelAndValue("Freight", textComponent));

        textComponent = createTextComponent("Total", "22");
        costTable.addRow(createRowWithLabelAndValue("Total", textComponent));

        textComponent = createTextComponent("Currency Code", "GBP");
        costTable.addRow(createRowWithLabelAndValue("Currency Code", textComponent));

        return costTable;
    }

    private TableRow createRowWithLabels(String... labels) {
        TableRow row = new TableRow();
        for (String label : labels) {
            row.addCell(new TableCell(label));
        }

        return row;
    }

    private TableRow createRowWithLabelAndValue(String label,
                                                TableTextComponent textComponent) {

        return createTableRow(label, textComponent);
    }

    private TableRow createTableRow(String label, TableTextComponent textComponent) {
        TableRow row = new TableRow();
        row.addCell(new TableCell(label));
        row.addCell(new TableCell(textComponent));
        return row;
    }

    private TableTextComponent createTextComponent(String fieldName, String fieldValue) {
        TableTextComponent textComponent = new TableTextComponent(fieldValue);
        textComponent.setName(fieldName);
        return textComponent;
    }

    public DocumentInfoView getDocInfoView() {
        return docInfoView;
    }
}
