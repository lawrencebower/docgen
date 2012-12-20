package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocumentBuilder;
import org.lawrencebower.docgen.web_logic.business.injection.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewBuilder;
import org.springframework.beans.factory.annotation.Autowired;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.CUSTOMER_ADDRESS;
import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.VENDOR_ADDRESS;

public class FDA_2887 {

    @Autowired
    private DocumentViewBuilder documentViewBuilder;
    @Autowired
    private OverlayDocumentBuilder documentBuilder;

    public static final String FDA_2887_NAME = "FDA_2887";

    public FDA_2887() {
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

        createTextComponent("Date of manufacture",
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

        createTextComponent("state reason",
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

        createTextComponent("List dates & restrictions",
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

        createTextComponent("Name and title of importer",
                   new DocCoordinates(264, 119, 332, 28),
                   false);
*/

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(FDA_2887_NAME,
                                       "C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\FDA-2877.pdf");
        documentViewBuilder.createDocument();
    }

    private void addTextBox(String name,
                            DocCoordinates coordinates,
                            boolean editable) {

        TextComponent textComponent = new TextComponent(name);
        textComponent.setCoordinates(coordinates);
        textComponent.setName(name);
//        textComponent.setRenderBorder(true);

        addComponent(textComponent);

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

    public DocumentView getDocumentView() {
        DocumentView documentView = documentViewBuilder.getDocumentView();
        OverlayDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }

}
