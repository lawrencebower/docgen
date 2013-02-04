package org.lawrencebower.docgen.web.test_examples.test_doc_1;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.web.test_examples.factory.DocumentFactoryTestImpl;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.Resource;

public class TestDocument1 {

    @Autowired
    private ComponentBuilder componentBuilder;
    @Autowired
    private DocumentViewBuilder documentViewBuilder;
    @Autowired
    private CustomDocumentBuilder documentBuilder;
    @javax.annotation.Resource
    @Qualifier("logoResource")
    private Resource logoResource;

    public static final String TEST_DOC_1_NAME = "test_doc_1";

    private void prepareComponents() {

        initDocumentBuilders();

        TableComponent logoTable = makeLogoTable();
        addComponent(logoTable);

        addNewLine();

        DocComponent component = componentBuilder.createTextComponentWithName(DocumentFactoryTestImpl.AUTO_MAPPED_EXAMPLE_FIELD);
        addViewableComponent(component);

        documentViewBuilder.setCopyNumber(2);

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(TEST_DOC_1_NAME);
        documentViewBuilder.createDocument();
    }

    private TableComponent makeLogoTable() {

        ImageComponent logo = new ImageComponent(logoResource);
        logo.setSize(70, 1);

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("logo table");
        tableBuilder.makeEmptyHeaderRowWithColSpans(2);

        LayoutRow row = tableBuilder.makeRow();

        LayoutCell logoCell = new LayoutCell(logo);
        row.addCell(logoCell);

        TextBlock sloganBlock = new TextBlock("DELIVERY NOTE",
                                              new FontInfo(FontInfo.DEFAULT_FONT,
                                                           24,
                                                           FontStyle.BOLD));

        TableTextComponent sloganComponent = new TableTextComponent(sloganBlock);
        sloganComponent.setAlignment(HorizontalAlignment.RIGHT);
        LayoutCell sloganCell = new LayoutCell(sloganComponent);
        sloganCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(sloganCell);

        tableBuilder.addRow(row);

        tableBuilder.setWidthPercentage(100);

        return tableBuilder.getTable();
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
