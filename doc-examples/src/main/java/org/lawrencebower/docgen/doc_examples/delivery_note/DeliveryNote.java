package org.lawrencebower.docgen.doc_examples.delivery_note;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
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
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewImpl;
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

        component = componentBuilder.createTextComponent("Practical products");
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

        component = componentBuilder.createTextComponent("Acme are pleased to confirm the delivery of:");
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

        TableTextComponent detailsComponent =
                componentBuilder.createTableTextComponent("observations", "THIS COMPLETES THE ORDER");

        TableCell detailsCell = new TableCell(detailsComponent);
        row.addCell(detailsCell);
        documentViewBuilder.addViewableComponent(detailsComponent);

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
        documentViewBuilder.addViewableComponent(nameComponent);
        headerRow.addCell(nameCell, 9);

        table.setHeaderRow(headerRow);

        TableRow companyRow = new TableRow();
        TableTextComponent companyComponent = new TableTextComponent("Acme ltd");
        companyComponent.setName(AutoMappedField.CUSTOMER_NAME.getName());
        TableCell companyCell = new TableCell(companyComponent);
        documentViewBuilder.addViewableComponent(companyComponent);
        companyRow.addCell(companyCell);
        table.addRow(companyRow);

        TableRow addressRow = new TableRow();
        TableTextComponent addressComponent = new TableTextComponent("36 BillyBob Street\nEssex");
        addressComponent.setName(AutoMappedField.CUSTOMER_ADDRESS.getName());
        TableCell addressCell = new TableCell(addressComponent);
        documentViewBuilder.addViewableComponent(addressComponent);
        addressRow.addCell(addressCell);
        table.addRow(addressRow);

        TableRow countryRow = new TableRow();
        TableTextComponent countryComponent = new TableTextComponent("UK");
        countryComponent.setName(AutoMappedField.CUSTOMER_COUNTRY.getName());
        TableCell countryCell = new TableCell(countryComponent);
        documentViewBuilder.addViewableComponent(countryComponent);
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
        documentView.setCopyNumber(5);
        return documentView;
    }
}
