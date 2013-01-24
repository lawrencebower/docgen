package org.lawrencebower.docgen.doc_examples.fcc_740;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocumentBuilder;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.*;

public class FCC_740 {

    @Autowired
    private DocumentViewBuilder documentViewBuilder;
    @Autowired
    private OverlayDocumentBuilder documentBuilder;

    @Autowired
    @Qualifier("signatureImagePath")
    private String signatureImagePath;

    @Autowired
    @Qualifier("fcc740Path")
    private String fcc740Path;

    public static final String FCC_740_NAME = "FCC_740";

    private FCC_740() {//force spring creation
    }

    private void prepareComponents() {

        initDocumentBuilders();

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

        addTextBox(AutoMappedField.VENDOR_NAME_AND_ADDRESS,
                   new DocCoordinates(25, 489, 175, 70),
                   true);

        addTextBox(CUSTOMER_ADDRESS,
                   new DocCoordinates(205, 489, 185, 70),
                   true);

        addTextBox(BUSINESS_ADDRESS,
                   new DocCoordinates(395, 489, 185, 70),
                   true);

        addTextBox(VENDOR_CONTACT_NAME,
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

        documentViewBuilder.setCopyNumber(5);

        documentViewBuilder.setCustomerAttributeFilters("USA");

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(FCC_740_NAME, fcc740Path);
        documentViewBuilder.createDocument();
    }

    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        OverlayDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);

        return documentView;
    }

    private void addTextBox(AutoMappedField mappedField,
                            DocCoordinates coordinates,
                            boolean editable) {

        addTextBox(mappedField.getName(),
                   coordinates,
                   editable);
    }

    private void addTextBox(String name,
                            DocCoordinates coordinates,
                            boolean editable) {

        FontInfo fontInfo = FontInfo.SMALL();
        TextBlock textBlock = new TextBlock(name, fontInfo);
        TableTextComponent textComponent = new TableTextComponent(textBlock);
        textComponent.setName(name);

        LayoutTableComponent table = new LayoutTableComponent();
        table.getHeaderRow().addCell(new LayoutHeaderCell());

        LayoutRow row = new LayoutRow();
        row.addCell(new LayoutCell(textComponent));
        table.addRow(row);

        table.setCoordinates(coordinates);
//        table.setRenderBorder(true);

        convertAndAddComponent(table);

        if (editable) {
            addViewableComponent(textComponent);
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
            addViewableComponent(checkComponent);
        }
    }

    private DocComponentView addViewableComponent(DocComponent checkComponent) {
        return documentViewBuilder.addViewableComponent(checkComponent);
    }

    private void convertAndAddComponent(DocComponent component) {
        documentBuilder.addComponent(component);
    }

}
