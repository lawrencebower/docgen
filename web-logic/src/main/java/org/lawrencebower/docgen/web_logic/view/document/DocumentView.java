package org.lawrencebower.docgen.web_logic.view.document;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.ArrayList;
import java.util.List;

public class DocumentView {

    private Document document;
    private List<DocComponentView> docComponentViews = new ArrayList<>();
    private int copyNumber = 1;//default

    private DocumentView() {//force spring instantiation
    }

    public void setDocument(Document document) {
        if (document == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

        this.document = document;
    }

    public String getName() {
        return document.getName();
    }

    public int getCopyNumber() {
        return copyNumber;
    }

    public void setCopyNumber(int copyNumber) {
        this.copyNumber = copyNumber;
    }

    public Document getDocument() {
        return document;
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
        return document.generatePDF();
    }
 }
