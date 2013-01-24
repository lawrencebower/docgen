package org.lawrencebower.docgen.doc_examples.commercial_invoice;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.*;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.WebTableHeaderRow;
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

        TableComponent invoiceTable = makeInvoiceTable();
        addComponent(invoiceTable);

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

        LayoutTableComponent invoiceTable = new LayoutTableComponent("invoice table");
        invoiceTable.setTablePadding(0);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        TableTextComponent headerComponent = new TableTextComponent(HorizontalAlignment.CENTER,
                                                                    "Commercial Invoice");
        LayoutHeaderCell headerCell = new LayoutHeaderCell(headerComponent);
        headerCell.setPadding(3);
        headerCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerCell.setColSpan(2);
        headerRow.addCell(headerCell);

        invoiceTable.setHeaderRow(headerRow);

        TableComponent shippedFrom1Table = makeShippedFrom1Table();
        TableComponent shippedFrom2Table = makeShippedFrom2Table();
        TableComponent shippedToTable = makeShippedToTable();
        TableComponent soldToTable = makeSoldToTable();

        LayoutRow row1 = new LayoutRow();
        row1.addCell(new LayoutCell(shippedFrom1Table));
        row1.addCell(new LayoutCell(shippedFrom2Table));

        invoiceTable.addRow(row1);

        LayoutRow row2 = new LayoutRow();
        row2.addCell(new LayoutCell(shippedToTable));
        row2.addCell(new LayoutCell(soldToTable));

        invoiceTable.addRow(row2);

        invoiceTable.setWidthPercentage(100);

        invoiceTable.setRenderBorder(true);

        return invoiceTable;
    }

    private TableComponent makeSoldToTable() {

        LayoutTableComponent table = new LayoutTableComponent("sold to");

        table.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedToCell = new LayoutHeaderCell("SOLD TO");
        shippedToCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedToCell.setColSpan(2);
        headerRow.addCell(shippedToCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(BUSINESS_NAME.getName(), "blah");
        LayoutRow row = createRowWithLabelAndValue("Name:", textComponent);
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

        LayoutTableComponent table = new LayoutTableComponent("shipped to");

        table.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedToCell = new LayoutHeaderCell("SHIPPED TO");
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

        LayoutTableComponent table = new LayoutTableComponent("shipped from 2");

        table.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell();
        shippedFromCell.setColSpan(2);
        headerRow.addCell(shippedFromCell);
        table.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("date", "29th May 2012");
        table.addRow(createRowWithLabelAndValue("Date:", textComponent));
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("refNumber", "154487945");
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

        LayoutTableComponent table = new LayoutTableComponent("shipped from");

        table.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell("SHIPPED FROM");
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
        LayoutTableComponent table = new LayoutTableComponent("package information");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell("Package Information");
        shippedFromCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerRow.addCell(shippedFromCell);
        table.setHeaderRow(headerRow);

        TableComponent productTable = makeProductTable();

        LayoutRow row = new LayoutRow();
        row.addCell(new LayoutCell(productTable));
        table.addRow(row);

        table.setWidthPercentage(100);
        table.setRenderBorder(true);
        table.setTablePadding(0);

        return table;
    }

    private TableComponent makeProductTable() {

        ViewTableComponent productTable = new ViewTableComponent("product table");

        WebTableHeaderRow headerRow = new WebTableHeaderRow();

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

        ViewHeaderCell totalCell = new ViewHeaderCell(TOTAL_VALUE_NAME);
        totalCell.setText("Total Value");
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

        LayoutTableComponent totalsTable = new LayoutTableComponent("totals table");

        totalsTable.setWidthPercentage(100);
        totalsTable.setTablePadding(0);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(false);
        headerRow.addCell(new LayoutHeaderCell(70));
        headerRow.addCell(new LayoutHeaderCell(30));
        totalsTable.setHeaderRow(headerRow);

        LayoutRow contentsRow = new LayoutRow();

        TableComponent weightTable = makeWeightTable();

        TableComponent costTable = makeCostTable();

        contentsRow.addCell(new LayoutCell(weightTable));
        contentsRow.addCell(new LayoutCell(costTable));

        totalsTable.addRow(contentsRow);

        return totalsTable;
    }

    private TableComponent makeWeightTable() {

        LayoutTableComponent weightTable = new LayoutTableComponent("weight table");

        weightTable.setWidthPercentage(100);
        weightTable.setRenderBorder(true);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell cell = new LayoutHeaderCell();
        cell.setColSpan(2);
        headerRow.addCell(cell);
        weightTable.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("Total Number of Packages:", "1");
        LayoutRow rowWithLabelAndValue = createRowWithLabelAndValue("Total Number of Packages:", textComponent);
        weightTable.addRow(rowWithLabelAndValue);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Total Weight:", "4kg");
        weightTable.addRow(createRowWithLabelAndValue("Total Weight:", textComponent));
        addViewableComponent(textComponent);

        return weightTable;
    }

    private TableComponent makeCostTable() {

        LayoutTableComponent costTable = new LayoutTableComponent("cost table");

        costTable.setWidthPercentage(100);
        costTable.setRenderBorder(true);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell cell = new LayoutHeaderCell();
        cell.setColSpan(2);
        headerRow.addCell(cell);
        costTable.setHeaderRow(headerRow);

        LayoutRow subTotalRow = new LayoutRow();
        subTotalRow.addCell(new LayoutCell("Subtotal"));
        String subTotalName = "subTotal";
        TableTextComponent subTotalComponent = createTableTextComponent(subTotalName);
        LayoutCell subTotalCell = new LayoutCell(subTotalComponent);
        subTotalRow.addCell(subTotalCell);
        costTable.addRow(subTotalRow);

        ComponentCalculation subtotalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                         Format.CURRENCY,
                                                                         TOTAL_VALUE_NAME);
        addViewableComponent(subTotalComponent, subtotalCalc);

        LayoutRow freightTotalRow = new LayoutRow();
        freightTotalRow.addCell(new LayoutCell("Freight"));
        String freightName = "freight";
        TableTextComponent freightComponent = createTableTextComponent(freightName, "0");
        LayoutCell freightCell = new LayoutCell(freightComponent);
        freightTotalRow.addCell(freightCell);
        costTable.addRow(freightTotalRow);

        LayoutRow totalRow = new LayoutRow();
        totalRow.addCell(new LayoutCell("Total"));
        String totalName = "total";
        TableTextComponent totalComponent = createTableTextComponent(totalName);
        LayoutCell totalCell = new LayoutCell(totalComponent);
        totalRow.addCell(totalCell);
        costTable.addRow(totalRow);

        ComponentCalculation totalCalc = new ComponentCalculationImpl(Operator.PLUS,
                                                                      Format.CURRENCY,
                                                                      TOTAL_VALUE_NAME);
        addViewableComponent(totalComponent, totalCalc);

        LayoutRow currencyRow = new LayoutRow();
        currencyRow.addCell(new LayoutCell("Currency Code"));
        String currencyName = "currency";
        TableTextComponent currencyComponent = createTableTextComponent(currencyName, "");
        LayoutCell currencyCell = new LayoutCell(currencyComponent);
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

        LayoutTableComponent sigTable = new LayoutTableComponent("signature table");
        sigTable.getHeaderRow().addCell(new LayoutHeaderCell());
        sigTable.getHeaderRow().addCell(new LayoutHeaderCell());

        LayoutRow row = new LayoutRow();

        LayoutCell imageCell = new LayoutCell(sigImage);
        row.addCell(imageCell);

        LayoutCell printedCell = new LayoutCell(printedSig);
        printedCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(printedCell);

        sigTable.addRow(row);

        sigTable.setWidthPercentage(25);

        return sigTable;
    }

    private TextComponent createTextComponent(String value) {
        return componentBuilder.createTextComponentWithValue(value);
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

    private LayoutRow createRowWithLabelAndValue(String label,
                                                TableTextComponent textComponent) {

        return createTableRow(label, textComponent);
    }

    private LayoutRow createTableRow(String label, TableTextComponent textComponent) {
        LayoutRow row = new LayoutRow();
        row.addCell(new LayoutCell(label));
        row.addCell(new LayoutCell(textComponent));
        return row;
    }

    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        CustomDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }
}
