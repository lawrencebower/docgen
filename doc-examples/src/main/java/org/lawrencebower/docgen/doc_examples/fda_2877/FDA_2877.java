package org.lawrencebower.docgen.doc_examples.fda_2877;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocumentBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.Resource;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.CUSTOMER_ADDRESS;
import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.VENDOR_ADDRESS;

public class FDA_2877 {

    @Autowired
    private DocumentViewBuilder documentViewBuilder;
    @Autowired
    private OverlayDocumentBuilder documentBuilder;

    @javax.annotation.Resource
    @Qualifier("fda2877Resource")
    private Resource fda2877Resource;

    public static final String FDA_2877_NAME = "FDA_2877";

    private FDA_2877() {//force spring creation
    }

    private void prepareComponents() {

        initDocumentBuilders();

        addTextBox("port of entry",
                   new DocCoordinates(27, 658, 285, 18),
                   false);

        addTextBox(VENDOR_ADDRESS.getName(),
                   new DocCoordinates(27, 592, 285, 57),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_NAME.getName(),
                   new DocCoordinates(27, 545, 170, 40),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_QUANTITY.getName(),
                   new DocCoordinates(198, 545, 114, 40),
                   true);

        addTextBox("entry number",
                   new DocCoordinates(312, 658, 156, 18),
                   false);

        addTextBox("date of entry",
                   new DocCoordinates(467, 658, 130, 18),
                   false);

        addTextBox(CUSTOMER_ADDRESS.getName(),
                   new DocCoordinates(312, 592, 285, 57),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_MODEL.getName(),
                   new DocCoordinates(312, 545, 285, 40),
                   true);

        addCheckBox("are not subject radiation",
                    new DocCoordinates(33, 519, 10, 10),
                    false,
                    true);

/*
        addCheckBox("X",
                    new DocCoordinates(48, 505, 10, 10),
                    false);

        createTextComponentWithValue("Date of manufacture",
                   new DocCoordinates(385, 509, 200, 10),
                   false);
*/

        addCheckBox("are excluded by the applicability clause",
                    new DocCoordinates(48, 493, 10, 10),
                    false,
                    true);

        addTextBox("It does not contain a LASER. Does not emit radiation.",
                   new DocCoordinates(175, 484, 270, 20),
                   false);

        /*
        addCheckBox("X",
                    new DocCoordinates(48, 470, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 457, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 445, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 433, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 409, 10, 10),
                    false);
            */
        addCheckBox("comply with performance standards",
                    new DocCoordinates(33, 380, 10, 10),
                    false,
                    true);

        addCheckBox("last annual report",
                    new DocCoordinates(48, 355, 10, 10),
                    false,
                    true);

        addTextBox("some kind of number",
                   new DocCoordinates(108, 346, 125, 20),
                   false);

        addTextBox("some manufacturer",
                   new DocCoordinates(255, 346, 270, 20),
                   false);

/*
        addCheckBox("X",
                    new DocCoordinates(48, 319, 10, 10),
                    false);

        createTextComponentWithValue("state reason",
                   new DocCoordinates(255, 321, 270, 10),
                   false);

        addCheckBox("X",
                    new DocCoordinates(33, 301, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 264, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 252, 10, 10),
                    false);

        createTextComponentWithValue("List dates & restrictions",
                   new DocCoordinates(257, 254, 270, 10),
                   false);

        addCheckBox("X",
                    new DocCoordinates(33, 232, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(48, 195, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(205, 195, 10, 10),
                    false);

        addCheckBox("X",
                    new DocCoordinates(358, 195, 10, 10),
                    false);

        createTextComponentWithValue("Name and title of importer",
                   new DocCoordinates(264, 119, 332, 28),
                   false);
*/

        documentViewBuilder.setProductAttributeFilters("gold");

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(FDA_2877_NAME, fda2877Resource);
        documentViewBuilder.createDocument();
    }

    private void addTextBox(String name,
                            DocCoordinates coordinates,
                            boolean editable) {

        FontInfo fontInfo = FontInfo.SMALL();
        TextBlock textBlock = new TextBlock(name, fontInfo);
        TableTextComponent textComponent = new TableTextComponent(textBlock);
        textComponent.setName(name);

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder();
        tableBuilder.makeEmptyHeaderRowWithColSpans(1);

        tableBuilder.addRowWithComponents(textComponent);

        tableBuilder.setCoordinates(coordinates);
//        table.setRenderBorder(true);

        LayoutTableComponent table = tableBuilder.getTable();

        addComponent(table);

        if (editable) {
            addViewableComponent(textComponent);
        }
    }

    private void addCheckBox(String name,
                             DocCoordinates coordinates,
                             boolean editable) {

        addCheckBox(name,
                    coordinates,
                    editable,
                    false);
    }

    private void addCheckBox(String name,
                             DocCoordinates coordinates,
                             boolean editable,
                             boolean selected) {

        CheckBoxComponent checkComponent = new CheckBoxComponent(selected);
        checkComponent.setCoordinates(coordinates);
        checkComponent.setName(name);

        addComponent(checkComponent);

        if (editable) {
            addViewableComponent(checkComponent);
        }
    }

    private void addViewableComponent(DocComponent component) {
        documentViewBuilder.addViewableComponent(component);
    }

    private void addComponent(DocComponent component) {
        documentBuilder.addComponent(component);
    }

    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        OverlayDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }

}
