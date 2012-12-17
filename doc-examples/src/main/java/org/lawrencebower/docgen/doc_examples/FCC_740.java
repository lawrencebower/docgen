package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponentFactory;
import org.lawrencebower.docgen.web_logic.business.injection.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.CheckBoxComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.TextComponentView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields.BUSINESS_ADDRESS;
import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields.CUSTOMER_ADDRESS;

public class FCC_740 {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    @Autowired
    private OverlayComponentFactory componentFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;
    @Autowired
    private DocumentViewFactory documentViewFactory;

    @Autowired
    @Qualifier("signatureImagePath")
    private String signatureImagePath;

    @Autowired
    @Qualifier("fcc740Path")
    private String fcc740Path;

    private DocumentView documentView;
    private OverlayDocument document;

    public static final String FCC_740_NAME = "FCC_740";

    public FCC_740() {
    }

    private void prepareComponents() {

        document = new OverlayDocument(FCC_740_NAME, pdfGenerator);
        document.setSourcePDF(fcc740Path);

        documentView = documentViewFactory.createDocumentView(document);

        documentView.setCopyNumber(5);

        addTextBox(DocumentInjectionField.PRODUCT_TARIFF_NUMBER.getName(),
                   new DocCoordinates(262, 645, 125, 20),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_QUANTITY.getName(),
                   new DocCoordinates(390, 645, 180, 20),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_NAME.getName(),
                   new DocCoordinates(25, 585, 135, 30),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_MODEL.getName(),
                   new DocCoordinates(165, 585, 70, 30),
                   true);

        addTextBox(DocumentInjectionField.PRODUCT_CUSTOMS_DESCRIPTION.getName(),
                   new DocCoordinates(368, 585, 210, 30),
                   true);

        addTextBox(AutoMappedFields.VENDOR_NAME_AND_ADDRESS,
                   new DocCoordinates(25, 489, 175, 70),
                   true);

        addTextBox(CUSTOMER_ADDRESS,
                   new DocCoordinates(205, 489, 185, 60),
                   true);

        addTextBox(BUSINESS_ADDRESS,
                   new DocCoordinates(395, 489, 185, 60),
                   true);

        addTextBox("Signature",
                   new DocCoordinates(25, 433, 265, 25),
                   true);

        ImageComponent signatureImage = new ImageComponent(signatureImagePath);
        signatureImage.setCoordinates(new DocCoordinates(295, 433, 190, 25));
        convertAndAddComponent(signatureImage);

        addTextBox("date",
                   new DocCoordinates(490, 433, 90, 25),
                   true);


        addCheckBox("Part 2",
                    new DocCoordinates(25, 320, 10, 10),
                    false,
                    true);


    }

    private void addTextBox(String name,
                            DocCoordinates coordinates,
                            boolean editable) {

        FontInfo fontInfo = FontInfo.SMALL();
        TextBlock textBlock = new TextBlock(name, fontInfo);
        TextComponent textComponent = new TextComponent(textBlock);
        textComponent.setCoordinates(coordinates);
        textComponent.setName(name);
        textComponent.setRenderBorder(true);

        convertAndAddComponent(textComponent);

        if (editable) {
            TextComponentView textComponentView = componentViewFactory.createTextComponentView(textComponent);
            documentView.addComponentView(textComponentView);
        }
    }

    private void addCheckBox(String name,
                             DocCoordinates coordinates,
                             boolean editable,
                             boolean selected) {

        CheckBoxComponent checkComponent = new CheckBoxComponent(selected);
        checkComponent.setCoordinates(coordinates);
        checkComponent.setName(name);
//        checkComponent.setRenderBorder(true);

        convertAndAddComponent(checkComponent);

        if (editable) {
            CheckBoxComponentView componentView = componentViewFactory.createCheckBoxComponentView(checkComponent);
            documentView.addComponentView(componentView);
        }
    }

    private void convertAndAddComponent(DocComponent component) {
        OverlayComponent overlayComponent = componentFactory.createOverlayComponent(component);
        document.addComponent(overlayComponent);
    }

    public DocumentView getDocumentView() {
        return documentView;
    }

}
