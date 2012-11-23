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
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentInfo;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.product_injection.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoViewFactory;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TextComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;

public class DeliveryNote {

    public static final Color ACME_BLUE = Color.decode("#F5FAFF");

    @Autowired
    private CustomPDFGenerator pdfGenerator;
    @Autowired
    private CustomComponentFactory componentFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;
    @Autowired
    DocumentInfoViewFactory docInfoViewFactory;

    private DocumentInfoView docInfoView;
    private CustomDocumentInfo docInfo;

    public static final String DELIVERY_NOTE_NAME = "Delivery note";

    public void prepareComponents() {

        docInfo = new CustomDocumentInfo(DELIVERY_NOTE_NAME, pdfGenerator);

        docInfoView = docInfoViewFactory.createDocumentInfoView(docInfo);

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

        TextComponent slogan = new TextComponent(sloganBlock);
        slogan.setAlignment(HorizontalAlignment.RIGHT);
        TableCell sloganCell = new TableCell(slogan);
        sloganCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(sloganCell);

        logoTable.setHeaderRow(row);

        logoTable.setWidthPercentage(100);

        convertAndAddComponent(logoTable);

        addNewLine();

        DocComponent yourReftable = makeYourRefTable();

        convertAndAddComponent(yourReftable);

        addTextBox("date",
                   "JULY 2 2012",
                   HorizontalAlignment.RIGHT);

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
        addressComponent.setName("vendorAddress");
        convertAndAddComponent(addressComponent);
        addTextComponentView(addressComponent,
                             AutoMappedComponent.VENDOR_ADDRESS);

        TextComponent phoneComponent = new TextComponent("Phone +44 (0) 1223 655577");
        phoneComponent.setName("vendorPhone");
        convertAndAddComponent(phoneComponent);
        addTextComponentView(phoneComponent,
                             AutoMappedComponent.VENDOR_PHONE);

        TextComponent emailComponent = new TextComponent("sales@acme.com");
        emailComponent.setName("vendorEmail");
        convertAndAddComponent(emailComponent);
        addTextComponentView(emailComponent,
                             AutoMappedComponent.VENDOR_EMAIL);

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

    private void convertAndAddComponent(DocComponent component) {
        CustomComponent customComponent = convertComponent(component);
        docInfo.addComponent(customComponent);
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
        desc.setName(ProductInjectionField.PRODUCT_NAME.getName());
        TableCell descriptionCell = new TableCell(desc);
        descriptionCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(descriptionCell, 80);

        table.setHeaderRow(headerRow);

/*
        TableRow row = new TableRow();

        TextBlock textBlock = new TextBlock("1", FontInfo.DEFAULT_BOLD());
        TextComponent numberComp = new TextComponent(textBlock);
        numberComp.setAlignment(HorizontalAlignment.CENTER);
        TableCell cell = new TableCell(numberComp);
        cell.setPadding(20);
        row.addCell(cell);

        TextFragment productFragment = new TextFragment("Z89914 ", FontInfo.DEFAULT_BOLD());
        TextFragment productFragment2 = new TextFragment("Z89914 Product Scanner");
        TextBlock productBlock = new TextBlock(productFragment, productFragment2);

        TableCell cell1 = new TableCell(new TextComponent(productBlock));
        cell1.setVerticalAlignment(VerticalAlignment.MIDDLE);
        cell1.setPadding(20);
        row.addCell(cell1);

        table.addRow(row);
*/

        table.setRenderBorder(true);

        return table;
    }

    private TableComponent makeDetailsTable() {
        TableComponent table = new TableComponent("Details table");
        table.setWidthPercentage(100);

        TableHeaderRow headerRow = new TableHeaderRow();

        TableCell detailsCell = new TableCell("DETAILS AND OBSERVATIONS");
        detailsCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(detailsCell);

        table.setHeaderRow(headerRow);

        TableRow row = new TableRow();

        TableCell cell = new TableCell("THIS COMPLETES THE ORDER");
        row.addCell(cell);

        table.addRow(row);

        table.setRenderBorder(true);

        return table;
    }

    private DocComponent makeToTable() {
        TableComponent table = new TableComponent("to table");

        TableHeaderRow headerRow = new TableHeaderRow();

        TableCell toCell = new TableCell("To");
        headerRow.addCell(toCell, 1);

        TableTextComponent addressComponent = new TableTextComponent("Lawrence Bower\n" +
                                                                     "39 York Street\n" +
                                                                     "Cambridge CB12PZ");
        addressComponent.setName("customerAddress");
        TableCell addressCell = new TableCell(addressComponent);

        addTextAreaComponent(addressComponent,
                             AutoMappedComponent.CUSTOMER_ADDRESS);

        headerRow.addCell(addressCell, 9);

        table.setHeaderRow(headerRow);

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

    public DocumentInfoView getDocInfo() {
        return docInfoView;
    }

    private CustomComponent convertComponent(DocComponent component) {
        return componentFactory.createCustomComponent(component);
    }

    private void addTextComponentView(TextComponent textComponent) {
        TextComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addProductTableComponentView(TableComponent tableComponent) {
        TableComponentView componentView = componentViewFactory.createTableComponentView(tableComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextComponentView(TableTextComponent textComponent, AutoMappedComponent autoMappedComponent) {
        TextComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        componentView.setAutoMappedComponent(autoMappedComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextAreaComponent(TableTextComponent textComponent,
                                      AutoMappedComponent autoMappedComponent) {
        DocComponentView componentView = componentViewFactory.createTextAreaComponentView(textComponent);
        componentView.setAutoMappedComponent(autoMappedComponent);
        docInfoView.addComponentView(componentView);
    }

    private void addTextComponentView(TextComponent textComponent,
                                      AutoMappedComponent autoMappedComponent) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(textComponent);
        componentView.setAutoMappedComponent(autoMappedComponent);
        docInfoView.addComponentView(componentView);
    }
}
