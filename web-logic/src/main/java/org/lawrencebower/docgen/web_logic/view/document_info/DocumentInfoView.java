package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public class DocumentInfoView {

    private DocumentInfo documentInfo;
    private List<DocComponentView> docComponentViews;

    public DocumentInfoView(DocumentInfo documentInfo) {
        if (documentInfo == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

        docComponentViews = new ArrayList<>();

        this.documentInfo = documentInfo;
    }

    public String getName() {
        return documentInfo.getName();
    }

    public DocumentInfo getDocumentInfo() {
        return documentInfo;
    }

    public void addComponentView(DocComponentView componentView) {
        docComponentViews.add(componentView);
    }

    public List<DocComponentView> getComponentViews() {
        return docComponentViews;
    }

    public List<DocComponentView> getComponentViewsWithName(String componentName) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocComponentView componentView : getComponentViews()) {
            String name = componentView.getName();
            if (name != null && name.equals(componentName)) {
                results.add(componentView);
            }
        }

        return results;
    }

    public PDFDocument generatePDF() {
        PDFDocument pdfDocument = documentInfo.generatePDF();
        return pdfDocument;
    }
}
