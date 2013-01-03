package org.lawrencebower.docgen.doc_examples.commercial_invoice;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculationImpl;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Format;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_logic.business.component_calculation.table.TableComponentCalculationImpl;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.*;

public class CommercialInvoice {

    @Autowired
    private ComponentBuilder componentBuilder;
    @Autowired
    private CustomDocumentBuilder documentBuilder;
    @Autowired
    DocumentViewBuilder documentViewBuilder;

    public static final String INVOICE_NAME = "invoice";
    public static final String TOTAL_VALUE_NAME = "totalValue";


    private void prepareComponents() {

        initDocumentBuilders();

        TableComponent addressTable = makeInvoiceTable();
        addComponent(addressTable);

        addNewLine();

        TableComponent packageInformationTable = makePackageInformationTable();

        addComponent(packageInformationTable);

        TableComponent totalsTable = makeTotalsTable();

        addComponent(totalsTable);

        addNewLine();

        TableComponent signatureTable = makeSignatureTable();

        addComponent(signatureTable);

        addComponent(new LineComponent(70));

        DocComponent component;

        component = createTextComponent("Signature of exporter (print and sign)\n" +
                                        "I declare all the information to be accurate and correct");
        addComponent(component);

        addNewLine();

        component = createTextComponent("date", "29th May 2012");
        addComponent(component);
        addViewableComponent(component);

        addComponent(new LineComponent(30));

        component = createTextComponent("Date");
        addComponent(component);

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(INVOICE_NAME);
        documentViewBuilder.createDocument();
    }

    private TableComponent makeInvoiceTable() {

        TableComponent invoiceTable = new TableComponent("invoice table");
        invoiceTable.setTablePadding(0);

        TableHeaderRow headerRow = new TableHeaderRow();

        TableTextComponent headerComponent = new TableTextComponent(HorizontalAlignment.CENTER,
                                                                    "Commercial Invoice");
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

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(BUSINESS_NAME.getName(), "blah");
        TableRow row = createRowWithLabelAndValue("Name:", textComponent);
        table.addRow(row);
        documentViewBuilder.addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_CONTACT_NAME.getName(), "David Davidson");

        row = createRowWithLabelAndValue("Contact Name:", textComponent);
        table.addRow(row);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_PHONE.getName(), "123456788");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_ADDRESS.getName(),
                                                 "Suites 11 & 12\n" +
                                                 "Church Farm,\n" +
                                                 "Maris Lane,\n" +
                                                 "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_COUNTRY.getName(), "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addViewableComponent(textComponent);

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

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(CUSTOMER_NAME.getName(), "Frank");
        table.addRow(createRowWithLabelAndValue("Name:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_CONTACT_NAME.getName(), "Billy Bob Bobson");
        table.addRow(createRowWithLabelAndValue("Contact Name:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_PHONE.getName(), "123456788");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_ADDRESS.getName(),
                                                 "Suites 11 & 12\n" +
                                                 "Church Farm,\n" +
                                                 "Maris Lane,\n" +
                                                 "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_COUNTRY.getName(), "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addViewableComponent(textComponent);

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

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("date", "29th May 2012");
        table.addRow(createRowWithLabelAndValue("Date:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Reference/Order No:", "154487945");
        table.addRow(createRowWithLabelAndValue("Reference/Order No:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Airbill Number:", "45678945");
        table.addRow(createRowWithLabelAndValue("Airbill Number:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Reason For Export:", "SOLD");
        table.addRow(createRowWithLabelAndValue("Reason For Export:", textComponent));

        textComponent = createTableTextComponent("Incoterms:", "FOB");
        table.addRow(createRowWithLabelAndValue("Incoterms:", textComponent));

        return table;
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

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(VENDOR_NAME.getName(), "Acme Ltd");
        table.addRow(createRowWithLabelAndValue("Name:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_TAX_ID.getName(), "154487945");
        table.addRow(createRowWithLabelAndValue("TAX/VAT NUMBER:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_CONTACT_NAME.getName(), "David Davidson");
        table.addRow(createRowWithLabelAndValue("Contact Name:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_PHONE.getName(), "+44 (0)1223 655577");
        table.addRow(createRowWithLabelAndValue("Phone:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_ADDRESS.getName(),
                                                 "Suites 11 & 12\n" +
                                                 "Church Farm,\n" +
                                                 "Maris Lane,\n" +
                                                 "Trumpington, CB29LG");
        table.addRow(createRowWithLabelAndValue("Company address:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_COUNTRY.getName(), "UNITED KINGDOM");
        table.addRow(createRowWithLabelAndValue("Country:", textComponent));
        addViewableComponent(textComponent);


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
        String quantityName = ProductInjectionField.PRODUCT_QUANTITY.getName();
        quantityComponent.setName(quantityName);
        TableCell quantityCell = new TableCell(quantityComponent);
        headerRow.addCell(quantityCell);

        TextComponent nameComponent = new TextComponent("Description");
        String productNameName = ProductInjectionField.PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION.getName();
        nameComponent.setName(productNameName);
        TableCell nameCell = new TableCell(nameComponent);
        headerRow.addCell(nameCell);

        TextComponent valueComponent = new TextComponent("Unit Value");
        String productValueName = ProductInjectionField.PRODUCT_VALUE.getName();
        valueComponent.setName(productValueName);
        TableCell valueCell = new TableCell(valueComponent);
        headerRow.addCell(valueCell);

        TextComponent originComponent = new TextComponent("Country of origin");
        String productOriginName = ProductInjectionField.PRODUCT_ORIGIN.getName();
        originComponent.setName(productOriginName);
        TableCell originCell = new TableCell(originComponent);
        headerRow.addCell(originCell);

        TextComponent totalValueComponent = new TextComponent("Total Value");
        totalValueComponent.setName(TOTAL_VALUE_NAME);
        TableCell totalCell = new TableCell(totalValueComponent);

        headerRow.addCell(totalCell);

        productTable.setHeaderRow(headerRow);

        productTable.setWidthPercentage(100);
        productTable.setRenderBorder(true);

        TableComponentCalculationImpl calculation = new TableComponentCalculationImpl(Operator.MULTIPLY,
                                                                              Format.CURRENCY,
                                                                              TOTAL_VALUE_NAME,
                                                                              quantityName,
                                                                              productValueName);
        addViewableComponent(productTable, calculation);

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

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("Total Number of Packages:", "1");
        weightTable.addRow(createRowWithLabelAndValue("Total Number of Packages:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Total Weight:", "4kg");
        weightTable.addRow(createRowWithLabelAndValue("Total Weight:", textComponent));
        addViewableComponent(textComponent);

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

        TableRow subTotalRow = new TableRow();
        subTotalRow.addCell(new TableCell("Subtotal"));
        String subTotalName = "subTotal";
        TableTextComponent subTotalComponent = createTableTextComponent(subTotalName);
        TableCell subTotalCell = new TableCell(subTotalComponent);
        subTotalRow.addCell(subTotalCell);
        costTable.addRow(subTotalRow);

        ComponentCalculation subtotalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                     Format.CURRENCY,
                                                                     TOTAL_VALUE_NAME);
        addViewableComponent(subTotalComponent, subtotalCalc);

        TableRow freightTotalRow = new TableRow();
        freightTotalRow.addCell(new TableCell("Freight"));
        String freightName = "freight";
        TableTextComponent freightComponent = createTableTextComponent(freightName, "0");
        TableCell freightCell = new TableCell(freightComponent);
        freightTotalRow.addCell(freightCell);
        costTable.addRow(freightTotalRow);

        TableRow totalRow = new TableRow();
        totalRow.addCell(new TableCell("Total"));
        String totalName = "total";
        TableTextComponent totalComponent = createTableTextComponent(totalName);
        TableCell totalCell = new TableCell(totalComponent);
        totalRow.addCell(totalCell);
        costTable.addRow(totalRow);

        ComponentCalculation totalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                  Format.CURRENCY,
                                                                  TOTAL_VALUE_NAME);
        addViewableComponent(totalComponent, totalCalc);

        TableRow currencyRow = new TableRow();
        currencyRow.addCell(new TableCell("Currency Code"));
        String currencyName = "currency";
        TableTextComponent currencyComponent = createTableTextComponent(currencyName, "");
        TableCell currencyCell = new TableCell(currencyComponent);
        currencyRow.addCell(currencyCell);
        costTable.addRow(currencyRow);

        addViewableComponent(currencyComponent);

        return costTable;
    }

    private TableComponent makeSignatureTable() {

        ImageComponent sigImage = new ImageComponent("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\signature.gif");
        sigImage.setSize(15, 15);

        TextComponent printedSig = new TextComponent("brian");
        printedSig.setAlignment(HorizontalAlignment.RIGHT);

        TableComponent sigTable = new TableComponent("signature table");

        TableHeaderRow row = new TableHeaderRow();

        TableCell imageCell = new TableCell(sigImage);
        row.addCell(imageCell);

        TableCell printedCell = new TableCell(printedSig);
        printedCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(printedCell);

        sigTable.setHeaderRow(row);

        sigTable.setWidthPercentage(25);

        return sigTable;
    }

    private TextComponent createTextComponent(String value) {
        return componentBuilder.createTextComponent(value);
    }

    private TextComponent createTextComponent(String name, String value) {
        return componentBuilder.createTextComponent(name, value);
    }

    private TableTextComponent createTableTextComponent(String name, String value) {
        return componentBuilder.createTableTextComponent(name, value);
    }

    private TableTextComponent createTableTextComponent(String name) {
        return componentBuilder.createTableTextComponent(name);
    }

    private void addComponent(DocComponent component) {
        documentBuilder.addComponent(component);
    }

    private void addViewableComponent(DocComponent component) {
        documentViewBuilder.addViewableComponent(component);
    }

    private void addNewLine() {
        NewLineComponent newLine = componentBuilder.createNewLine();
        addComponent(newLine);
    }

    private void addViewableComponent(DocComponent component,
                                      ComponentCalculation calculation) {
        documentViewBuilder.addViewableComponent(component, calculation);
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

    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        CustomDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }
}
