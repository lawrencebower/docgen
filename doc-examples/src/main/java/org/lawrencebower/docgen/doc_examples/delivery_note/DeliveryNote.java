package org.lawrencebower.docgen.doc_examples.delivery_note;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.doc_examples.delivery_note.components.*;
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

        FontInfo fontInfo = FontInfo.DEFAULT_BOLD();
        TextBlock textBlock = new TextBlock("Acme Ltd", fontInfo);
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

        LogoTableBuilder logoTableBuilder = new LogoTableBuilder();

        return logoTableBuilder.buildLogoTable();
    }

    private TableComponent makeMainOrderTable() {

        MainOrderTableBuilder mainOrderTableBuilder = new MainOrderTableBuilder();

        return mainOrderTableBuilder.buildMainOrderTable();
    }

    private TableComponent makeDetailsTable() {

        DetailsTableBuilder detailsTableBuilder = new DetailsTableBuilder(componentBuilder, documentViewBuilder);

        return detailsTableBuilder.buildDetailsTable();
    }

    private DocComponent makeToTable() {

        ToTableBuilder toTableBuilder = new ToTableBuilder(documentViewBuilder);

        return toTableBuilder.buildToTable();
    }

    private DocComponent makeYourRefTable() {

        YourRefTableBuilder yourRefTableBuilder = new YourRefTableBuilder(documentViewBuilder);

        return yourRefTableBuilder.buildYourRefTable();
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
