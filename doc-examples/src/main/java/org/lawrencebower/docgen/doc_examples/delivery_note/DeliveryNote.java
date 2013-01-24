package org.lawrencebower.docgen.doc_examples.delivery_note;

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
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.*;

public class DeliveryNote {

    public static final Color ACME_BLUE = Color.decode("#F5FAFF");

    @Autowired
    private ComponentBuilder componentBuilder;
    @Autowired
    private DocumentViewBuilder documentViewBuilder;
    @Autowired
    private CustomDocumentBuilder documentBuilder;

    public static final String DELIVERY_NOTE_NAME = "Delivery note";

    private void prepareComponents() {

        initDocumentBuilders();

        TableComponent logoTable = makeLogoTable();
        addComponent(logoTable);

        addNewLine();

        makeHeader();

        makeBody();

        addNewLine();

        makeFooter();

        documentViewBuilder.setCopyNumber(5);

//        documentViewBuilder.setCustomerAttributeFilters("USA_EAST_COAST");

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(DELIVERY_NOTE_NAME);
        documentViewBuilder.createDocument();
    }

    private void makeHeader() {

        DocComponent yourReftable = makeYourRefTable();
        addComponent(yourReftable);

        DocComponent component = componentBuilder.createTextComponent("date",
                                                                      "JULY 2 2012",
                                                                      HorizontalAlignment.RIGHT);
        addViewableComponent(component);

        TextBlock textBlock = new TextBlock("Acme Ltd", FontInfo.DEFAULT_BOLD());
        TextComponent textComponent = new TextComponent(textBlock);
        addComponent(textComponent);

        component = componentBuilder.createTextComponentWithValue("Practical products");
        addComponent(component);

        addNewLine();

        String addressValue = "Suites 11 and 12, Church Farm\n" +
                              "Maris Lane, Trumpington, CB29LG, UK";
        component = componentBuilder.createTextComponent(VENDOR_ADDRESS.getName(),
                                                         addressValue);
        addViewableComponent(component);

        component = componentBuilder.createTextComponent(VENDOR_PHONE.getName(),
                                                         "Phone +44 (0) 1223 1235455");
        addViewableComponent(component);

        component = componentBuilder.createTextComponent(VENDOR_EMAIL.getName(),
                                                         "sales@acme.com");
        addViewableComponent(component);

        addNewLine();

        DocComponent toTable = makeToTable();
        addComponent(toTable);
    }

    private void makeBody() {

        DocComponent component;

        addNewLine();

        component = componentBuilder.createTextComponentWithValue("Acme are pleased to confirm the delivery of:");
        addComponent(component);

        addNewLine();

        TableComponent table = makeMainOrderTable();
        addViewableComponent(table);
    }

    private void makeFooter() {

        DocComponent component;

        component = makeDetailsTable();

        addComponent(component);

        addNewLine();
        addNewLine();

        component = componentBuilder.createTextComponent("thanks for business",
                                                         "THANK YOU FOR YOUR BUSINESS",
                                                         HorizontalAlignment.CENTER);
        addViewableComponent(component);
    }

    private TableComponent makeLogoTable() {

        ImageComponent logo = new ImageComponent("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\logo.png");
        logo.setSize(70, 1);

        LayoutTableComponent logoTable = new LayoutTableComponent("logo table");

        LayoutHeaderRow row = new LayoutHeaderRow();
        row.setRenderHeader(true);

        LayoutHeaderCell logoCell = new LayoutHeaderCell(logo);
        row.addCell(logoCell);

        TextBlock sloganBlock = new TextBlock("DELIVERY NOTE",
                                              new FontInfo(FontInfo.DEFAULT_FONT,
                                                           24,
                                                           FontStyle.BOLD));

        TableTextComponent slogan = new TableTextComponent(sloganBlock);
        slogan.setAlignment(HorizontalAlignment.RIGHT);
        LayoutHeaderCell sloganCell = new LayoutHeaderCell(slogan);
        sloganCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(sloganCell);

        logoTable.setHeaderRow(row);

        logoTable.setWidthPercentage(100);

        return logoTable;
    }

    private TableComponent makeMainOrderTable() {

        ViewTableComponent table = new ViewTableComponent("Items table");
        table.setWidthPercentage(100);

        WebTableHeaderRow headerRow = new WebTableHeaderRow();

        TextComponent qty = new TextComponent(HorizontalAlignment.CENTER, "QTY");
        ViewHeaderCell qtyCell = new ViewHeaderCell(ProductInjectionField.PRODUCT_QUANTITY.getName());
        qtyCell.setText("QTY");
        qtyCell.setColumnWidth(20);
        qtyCell.setBackgroundColor(ACME_BLUE);
        qtyCell.setComponent(qty);
        headerRow.addCell(qtyCell);

        ViewHeaderCell descriptionCell = new ViewHeaderCell(ProductInjectionField.PRODUCT_NAME_AND_DESCRIPTION.getName());
        descriptionCell.setText("DESCRIPTION");
        descriptionCell.setColumnWidth(80);
        descriptionCell.setBackgroundColor(ACME_BLUE);
        TextComponent desc = new TextComponent(HorizontalAlignment.CENTER, "DESCRIPTION");
        descriptionCell.setComponent(desc);
        headerRow.addCell(descriptionCell);

        table.setHeaderRow(headerRow);

        table.setRenderBorder(true);

        return table;
    }

    private TableComponent makeDetailsTable() {
        LayoutTableComponent table = new LayoutTableComponent("Details table");
        table.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        LayoutHeaderCell headerCell = new LayoutHeaderCell("DETAILS AND OBSERVATIONS");
        headerCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(headerCell);

        table.setHeaderRow(headerRow);

        LayoutRow row = new LayoutRow();

        TableTextComponent detailsComponent =
                componentBuilder.createTableTextComponent("observations", "THIS COMPLETES THE ORDER");

        LayoutCell detailsCell = new LayoutCell(detailsComponent);
        row.addCell(detailsCell);
        documentViewBuilder.addViewableComponent(detailsComponent);

        table.addRow(row);

        table.setRenderBorder(true);

        return table;
    }

    private DocComponent makeToTable() {

        LayoutTableComponent table = new LayoutTableComponent("to table");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        LayoutHeaderCell toCell = new LayoutHeaderCell("To");
        toCell.setColumnWidth(1);
        toCell.setRowSpan(4);
        headerRow.addCell(toCell);

        TableTextComponent nameComponent = new TableTextComponent("Lawrence Bower");
        nameComponent.setName(AutoMappedField.CUSTOMER_CONTACT_NAME.getName());
        LayoutHeaderCell nameCell = new LayoutHeaderCell(nameComponent);
        documentViewBuilder.addViewableComponent(nameComponent);
        nameCell.setColumnWidth(9);
        headerRow.addCell(nameCell);

        table.setHeaderRow(headerRow);

        LayoutRow companyRow = new LayoutRow();
        TableTextComponent companyComponent = new TableTextComponent("Acme ltd");
        companyComponent.setName(AutoMappedField.CUSTOMER_NAME.getName());
        LayoutCell companyCell = new LayoutCell(companyComponent);
        documentViewBuilder.addViewableComponent(companyComponent);
        companyRow.addCell(companyCell);
        table.addRow(companyRow);

        LayoutRow addressRow = new LayoutRow();
        TableTextComponent addressComponent = new TableTextComponent("36 BillyBob Street\nEssex");
        addressComponent.setName(AutoMappedField.CUSTOMER_ADDRESS.getName());
        LayoutCell addressCell = new LayoutCell(addressComponent);
        documentViewBuilder.addViewableComponent(addressComponent);
        addressRow.addCell(addressCell);
        table.addRow(addressRow);

        LayoutRow countryRow = new LayoutRow();
        TableTextComponent countryComponent = new TableTextComponent("UK");
        countryComponent.setName(AutoMappedField.CUSTOMER_COUNTRY.getName());
        LayoutCell countryCell = new LayoutCell(countryComponent);
        documentViewBuilder.addViewableComponent(countryComponent);
        countryRow.addCell(countryCell);
        table.addRow(countryRow);

        table.setRenderBorder(false);

        return table;
    }

    private DocComponent makeYourRefTable() {
        LayoutTableComponent table = new LayoutTableComponent("your ref table");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        TableTextComponent yourRefComponent = new TableTextComponent("your ref -");
        yourRefComponent.setAlignment(HorizontalAlignment.RIGHT);
        LayoutHeaderCell toCell = new LayoutHeaderCell(yourRefComponent);
        headerRow.addCell(toCell);

        TextBlock refNoBlock = new TextBlock("PO 42464", FontInfo.DEFAULT_BOLD());
        TableTextComponent refNumberComponent = new TableTextComponent(refNoBlock);
        refNumberComponent.setName("refNumber");
        refNumberComponent.setAlignment(HorizontalAlignment.RIGHT);
        LayoutHeaderCell addressCell = new LayoutHeaderCell(refNumberComponent);

        documentViewBuilder.addViewableComponent(refNumberComponent);

        headerRow.addCell(addressCell);

        table.setHeaderRow(headerRow);

        table.setRenderBorder(false);

        table.setAlignment(HorizontalAlignment.RIGHT);

        table.setWidthPercentage(20);

        table.setTablePadding(0);

        return table;
    }

    private void addNewLine() {
        NewLineComponent newLine = componentBuilder.createNewLine();
        addComponent(newLine);
    }

    private void addComponent(DocComponent component) {
        documentBuilder.addComponent(component);
    }

    private void addViewableComponent(DocComponent component) {
        addComponent(component);
        documentViewBuilder.addViewableComponent(component);
    }

    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        CustomDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }
}
