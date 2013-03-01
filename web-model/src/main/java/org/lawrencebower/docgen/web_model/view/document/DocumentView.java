package org.lawrencebower.docgen.web_model.view.document;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface DocumentView {
    void setDocument(Document document);

    String getName();

    void setNameExtension(String nameExtension);

    String getNameExtension();

    int getCopyNumber();

    Document getDocument();

    List<DocComponentView> getComponentViews();

    List<DocComponentView> getComponentViewsWithName(String componentName);

    PDFDocument generatePDF();

    void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo);

    boolean hasDocumentInjectionFields();

    List<DocumentView> injectDocuments(List<DocumentInjectionInfo> injectionInfos);

    boolean isContactAttributesMatch(ContactView contact);

    boolean isProductAttributesMatch(ProductView product);

    void copyComponentViews(DocumentView documentToCopy);
}
