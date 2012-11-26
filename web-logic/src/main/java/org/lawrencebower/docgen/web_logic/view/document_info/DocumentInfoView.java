package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;

import java.util.ArrayList;
import java.util.List;

public class DocumentInfoView {

    private DocumentInfo documentInfo;
    private List<DocComponentView> docComponentViews = new ArrayList<>();

    private DocumentInfoView() {//force spring instantiation
    }

    public void setDocumentInfo(DocumentInfo documentInfo) {
        if (documentInfo == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

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

    /**
     * returns all ComponentViews matching supplied name
     */
    public List<DocComponentView> getComponentViewsWithName(String componentName) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocComponentView componentView : getComponentViews()) {
            String name = componentView.getName();
            if ((name != null) && name.equals(componentName)) {
                results.add(componentView);
            }
        }

        return results;
    }

    public PDFDocument generatePDF() {
        return documentInfo.generatePDF();
    }
 }
