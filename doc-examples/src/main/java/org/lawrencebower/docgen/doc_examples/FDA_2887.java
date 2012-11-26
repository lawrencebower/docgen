package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.CheckBoxComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.TextComponentView;
import org.springframework.beans.factory.annotation.Autowired;

public class FDA_2887 {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    @Autowired
    private DocComponentViewFactory componentViewFactory;
    @Autowired
    DocumentViewFactory documentViewFactory;

    private DocumentView documentView;

    public FDA_2887() {
    }

    public void prepareComponents() {

        OverlayDocument document = new OverlayDocument("FDA-2887", pdfGenerator);
        document.setSourcePDF("C:\\GitHub\\use_cases\\src\\main\\resources\\FDA-2877.pdf");

        documentView = documentViewFactory.createDocumentInfoView(document);

        addTextBox("port of entry",
                   new DocCoordinates(27, 658, 285, 18),
                   documentView,
                   true);

        addTextBox("name & address of manufacture",
                   new DocCoordinates(27, 592, 285, 57),
                   documentView,
                   true);

        addTextBox("product description",
                   new DocCoordinates(27, 545, 170, 40),
                   documentView,
                   true);

        addTextBox("quantity",
                   new DocCoordinates(198, 545, 114, 40),
                   documentView,
                   true);

        addTextBox("entry number",
                   new DocCoordinates(312, 658, 156, 18),
                   documentView,
                   true);

        addTextBox("date of entry",
                   new DocCoordinates(467, 658, 130, 18),
                   documentView,
                   true);

        addTextBox("name and address of importer",
                   new DocCoordinates(312, 592, 285, 57),
                   documentView,
                   true);

        addTextBox("model numbers",
                   new DocCoordinates(312, 545, 285, 40),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(33, 519, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 505, 10, 10),
                    documentView,
                    true);

        addTextBox("Date of manufacture",
                   new DocCoordinates(385, 509, 200, 10),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(48, 493, 10, 10),
                    documentView,
                    true);

        addTextBox("reason for exclusion",
                   new DocCoordinates(175, 484, 270, 10),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(48, 470, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 457, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 445, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 433, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 409, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(33, 380, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 355, 10, 10),
                    documentView,
                    true);

        addTextBox("accession number",
                   new DocCoordinates(108, 346, 125, 10),
                   documentView,
                   true);

        addTextBox("name of manufacturer of record",
                   new DocCoordinates(255, 346, 270, 10),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(48, 319, 10, 10),
                    documentView,
                    true);

        addTextBox("state reason",
                   new DocCoordinates(255, 321, 270, 10),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(33, 301, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 264, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 252, 10, 10),
                    documentView,
                    true);

        addTextBox("List dates & restrictions",
                   new DocCoordinates(257, 254, 270, 10),
                   documentView,
                   true);

        addCheckBox("X",
                    new DocCoordinates(33, 232, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(48, 195, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(205, 195, 10, 10),
                    documentView,
                    true);

        addCheckBox("X",
                    new DocCoordinates(358, 195, 10, 10),
                    documentView,
                    true);

        addTextBox("Name and title of importer",
                   new DocCoordinates(264, 119, 332, 28),
                   documentView,
                   true);

    }

    private void addTextBox(String name,
                            DocCoordinates coordinates,
                            DocumentView documentView,
                            boolean editable) {

        TextComponent textComponent = new TextComponent(name);
        textComponent.setCoordinates(coordinates);
        textComponent.setName(name);
        TextComponentView textComponentView = componentViewFactory.createTextComponentView(textComponent);
        documentView.addComponentView(textComponentView);
    }

    private void addCheckBox(String name,
                             DocCoordinates coordinates,
                             DocumentView documentView,
                             boolean editable) {

        CheckBoxComponent textComponent = new CheckBoxComponent(true);
        textComponent.setCoordinates(coordinates);
        textComponent.setName(name);
        CheckBoxComponentView componentView = componentViewFactory.createCheckBoxComponentView(textComponent);
        documentView.addComponentView(componentView);
    }

    public DocumentView getDocumentView() {
        return documentView;
    }

    public void setComponentValuesAndRenderBorder() {
        for (DocComponentView component : documentView.getComponentViews()) {
//            component.setRenderBorder(true);
            DocComponent docComponent = component.getDocComponent();
            if (docComponent instanceof TextComponent) {
                ((TextComponent) docComponent).setText(docComponent.getName());
            }
        }

    }
}
