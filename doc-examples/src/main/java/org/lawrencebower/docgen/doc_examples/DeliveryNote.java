package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.TableComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.TextComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.*;

public class DeliveryNote {

    public static final Color ACME_BLUE = Color.decode("#F5FAFF");

    @Autowired
    private CustomPDFGenerator pdfGenerator;
    @Autowired
    private CustomComponentFactory componentFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;
    @Autowired
    DocumentViewFactory documentViewFactory;

    private DocumentView documentView;
    private CustomDocument document;

    public static final String DELIVERY_NOTE_NAME = "Delivery note";

    private void prepareComponents() {

        document = new CustomDocument(DELIVERY_NOTE_NAME, pdfGenerator);

        documentView = documentViewFactory.createDocumentView(document);

        documentView.setCopyNumber(2);

        TableComponent logoTable = makeLogoTable();

        convertAndAddComponent(logoTable);

        addNewLine();

        DocComponent yourReftable = makeYourRefTable();

        convertAndAddComponent(yourReftable);

        addTextBox("date",
                   new TextBlock("JULY 2 2012"),
                   HorizontalAlignment.RIGHT,
                   true);

        addTextBox("Acme tag",
                   new TextBlock("Acme Ltd", FontInfo.DEFAULT_BOLD()),
                   HorizontalAlignment.LEFT,
                   false);

        addTextBox("innovation",
                   "Practical products",
                   HorizontalAlignment.LEFT);

        addNewLine();

        TextComponent addressComponent = new TextComponent("Suites 11 and 12, Church Farm\n" +
                                                           "Maris Lane, Trumpington, CB29LG, UK");
        addressComponent.setName(VENDOR_ADDRESS.getName());
        convertAndAddComponent(addressComponent);
        addTextComponentView(addressComponent);

        TextComponent phoneComponent = new TextComponent("Phone +44 (0) 1223 655577");
        phoneComponent.setName(VENDOR_PHONE.getName());
        convertAndAddComponent(phoneComponent);
        addTextComponentView(phoneComponent);

        TextComponent emailComponent = new TextComponent("sales@acme.com");
        emailComponent.setName(VENDOR_EMAIL.getName());
        convertAndAddComponent(emailComponent);
        addTextComponentView(emailComponent);

        addNewLine();

        DocComponent toTable = makeToTable();
        convertAndAddComponent(toTable);

        addNewLine();

        addTextBox("delivery of",
                   "Acme are pleased to confirm the delivery of:",
                   HorizontalAlignment.LEFT);

        addNewLine();

        TableComponent table = makeMainOrderTable();

        convertAndAddComponent(table);
        addProductTableComponentView(table);

        addNewLine();

        TableComponent detailsTable = makeDetailsTable();

        convertAndAddComponent(detailsTable);

        addNewLine();
        addNewLine();

        addTextBox("thanks for business",
                   new TextBlock("THANK YOU FOR YOUR BUSINESS", FontInfo.DEFAULT()),
                   HorizontalAlignment.CENTER,
                   false);

    }

    private TableComponent makeLogoTable() {

        ImageComponent logo = new ImageComponent("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\logo.png");
        logo.setSize(70, 1);

        TableComponent logoTable = new TableComponent("logo table");

        TableHeaderRow row = new TableHeaderRow();

        TableCell logoCell = new TableCell(logo);
        row.addCell(logoCell);

        TextBlock sloganBlock = new TextBlock("DELIVERY NOTE",
                                              new FontInfo(FontInfo.DEFAULT_FONT,
                                                           24,
                                                           FontStyle.BOLD));

        TableTextComponent slogan = new TableTextComponent(sloganBlock);
        slogan.setAlignment(HorizontalAlignment.RIGHT);
        TableCell sloganCell = new TableCell(slogan);
        sloganCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(sloganCell);

        logoTable.setHeaderRow(row);

        logoTable.setWidthPercentage(100);

        return logoTable;
    }

    private void convertAndAddComponent(DocComponent component) {
        CustomComponent customComponent = convertComponent(component);
        document.addComponent(customComponent);
    }

    private TableComponent makeMainOrderTable() {
        TableComponent table = new TableComponent("Items table");
        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();

        TextComponent qty = new TextComponent(HorizontalAlignment.CENTER, "QTY");
        qty.setName(ProductInjectionField.PRODUCT_QUANTITY.getName());
        TableCell qtyCell = new TableCell(qty);
        qtyCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(qtyCell, 20);

        TextComponent desc = new TextComponent(HorizontalAlignment.CENTER,
                                               "DESCRIPTION");
        desc.setName(ProductInjectionField.PRODUCT_NAME_AND_DESCRIPTION.getName());
        TableCell descriptionCell = new TableCell(desc);
        descriptionCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(descriptionCell, 80);

        table.setHeaderRow(headerRow);

        table.setRenderBorder(true);

        return table;
    }

    private TableComponent makeDetailsTable() {
        TableComponent table = new TableComponent("Details table");
        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();

        TableCell headerCell = new TableCell("DETAILS AND OBSERVATIONS");
        headerCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(headerCell);

        table.setHeaderRow(headerRow);

        TableRow row = new TableRow();

        TableTextComponent detailsComponent = new TableTextComponent("THIS COMPLETES THE ORDER");
        TableCell detailsCell = new TableCell(detailsComponent);
        row.addCell(detailsCell);
        addTextComponentView(detailsComponent);

        table.addRow(row);

        table.setRenderBorder(true);

        return table;
    }

    private DocComponent makeToTable() {
        TableComponent table = new TableComponent("to table");

        TableHeaderRow headerRow = new TableHeaderRow();

        TableCell toCell = new TableCell("To");
        toCell.setRowSpan(4);
        headerRow.addCell(toCell, 1);

        TableTextComponent nameComponent = new TableTextComponent("Lawrence Bower");
        nameComponent.setName(AutoMappedField.CUSTOMER_CONTACT_NAME.getName());
        TableCell nameCell = new TableCell(nameComponent);
        addTextAreaComponent(nameComponent);
        headerRow.addCell(nameCell, 9);

        table.setHeaderRow(headerRow);

        TableRow companyRow = new TableRow();
        TableTextComponent companyComponent = new TableTextComponent("Acme ltd");
        companyComponent.setName(AutoMappedField.CUSTOMER_NAME.getName());
        TableCell companyCell = new TableCell(companyComponent);
        addTextAreaComponent(companyComponent);
        companyRow.addCell(companyCell);
        table.addRow(companyRow);

        TableRow addressRow = new TableRow();
        TableTextComponent addressComponent = new TableTextComponent("36 BillyBob Street\nEssex");
        addressComponent.setName(AutoMappedField.CUSTOMER_ADDRESS.getName());
        TableCell addressCell = new TableCell(addressComponent);
        addTextAreaComponent(addressComponent);
        addressRow.addCell(addressCell);
        table.addRow(addressRow);

        TableRow countryRow = new TableRow();
        TableTextComponent countryComponent = new TableTextComponent("UK");
        countryComponent.setName(AutoMappedField.CUSTOMER_COUNTRY.getName());
        TableCell countryCell = new TableCell(countryComponent);
        addTextAreaComponent(countryComponent);
        countryRow.addCell(countryCell);
        table.addRow(countryRow);

        table.setRenderBorder(false);

        return table;
    }

    private DocComponent makeYourRefTable() {
        TableComponent table = new TableComponent("your ref table");

        TableHeaderRow headerRow = new TableHeaderRow();

        TableTextComponent yourRefComponent = new TableTextComponent("your ref -");
        yourRefComponent.setAlignment(HorizontalAlignment.RIGHT);
        TableCell toCell = new TableCell(yourRefComponent);
        headerRow.addCell(toCell);

        TextBlock refNoBlock = new TextBlock("PO 42464", FontInfo.DEFAULT_BOLD());
        TableTextComponent refNumberComponent = new TableTextComponent(refNoBlock);
        refNumberComponent.setName("refNumber");
        refNumberComponent.setAlignment(HorizontalAlignment.RIGHT);
        TableCell addressCell = new TableCell(refNumberComponent);

        addTextComponentView(refNumberComponent);

        headerRow.addCell(addressCell);

        table.setHeaderRow(headerRow);

        table.setRenderBorder(false);

        table.setAlignment(HorizontalAlignment.RIGHT);

        table.setWidthPercentage(20);

        table.setTablePadding(0);

        return table;
    }

    private void setDefaultAlignment(TableCell cell) {
        cell.setVerticalAlignment(VerticalAlignment.MIDDLE);
    }

    private void addNewLine() {
        convertAndAddComponent(new NewLineComponent());
    }

    private void addTextBox(String name,
                            String value,
                            HorizontalAlignment alignment) {

        TextBlock textBlock = new TextBlock(value);
        addTextBox(name,
                   textBlock,
                   alignment,
                   false);
    }

    private void addTextBox(String name,
                            TextBlock value,
                            HorizontalAlignment alignment,
                            boolean editable) {

        TextComponent textComponent = new TextComponent(value);
        textComponent.setName(name);
        textComponent.setAlignment(alignment);

        convertAndAddComponent(textComponent);

        if (editable) {
            addTextComponentView(textComponent);
        }
    }

    public DocumentView getDocumentView() {
        return documentView;
    }

    private CustomComponent convertComponent(DocComponent component) {
        return componentFactory.createCustomComponent(component);
    }

    private void addTextComponentView(TextComponent textComponent) {
        TextComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        documentView.addComponentView(componentView);
    }

    private void addProductTableComponentView(TableComponent tableComponent) {
        TableComponentView componentView = componentViewFactory.createTableComponentView(tableComponent);
        documentView.addComponentView(componentView);
    }

    private void addTextComponentView(TableTextComponent textComponent) {
        TextComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        documentView.addComponentView(componentView);
    }

    private void addTextAreaComponent(TableTextComponent textComponent) {
        DocComponentView componentView = componentViewFactory.createTextAreaComponentView(textComponent);
        documentView.addComponentView(componentView);
    }
}
