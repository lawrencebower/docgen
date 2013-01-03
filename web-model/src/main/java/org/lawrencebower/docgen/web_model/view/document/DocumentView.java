package org.lawrencebower.docgen.web_model.view.document;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.util.List;

public interface DocumentView {
    void setDocument(Document document);

    String getName();

    void setNameExtension(String nameExtension);

    String getNameExtension();

    int getCopyNumber();

    void setCopyNumber(int copyNumber);

    Document getDocument();

    void addComponentView(DocComponentView componentView);

    List<DocComponentView> getComponentViews();

    List<DocComponentView> getComponentViewsWithName(String componentName);

    PDFDocument generatePDF();

    void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo);

    boolean hasDocumentInjectionFields();

    List<DocumentView> injectDocuments(List<DocumentInjectionInfo> injectionInfos);
}
