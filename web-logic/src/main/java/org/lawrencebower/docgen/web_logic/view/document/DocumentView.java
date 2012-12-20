package org.lawrencebower.docgen.web_logic.view.document;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.injection.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class DocumentView {

    @Autowired
    ViewFactory viewFactory;

    private Document document;
    private List<DocComponentView> docComponentViews = new ArrayList<>();
    private int copyNumber = 1;//default
    private String nameExtension;

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

    public void setNameExtension(String nameExtension) {
        this.nameExtension = nameExtension;
    }

    public String getNameExtension() {
        return nameExtension;
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

    public void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo) {

        for (DocComponentView docComponentView : getComponentViews()) {
            docComponentView.setDocumentInjectionFields(injectionInfo);
        }

    }

    public boolean hasDocumentInjectionFields() {

        boolean hasDocumentInjectionField = false;

        for (DocComponentView docComponentView : getComponentViews()) {
            if (docComponentView.isDocumentInjection()) {
                hasDocumentInjectionField = true;
            }
        }

        return hasDocumentInjectionField;
    }

    /**
     * This is not a field-for-field copy.
     */
    public DocumentView copyComponentViews() {
        String docName = document.getName();
        DocumentView newDocument = viewFactory.createDocument(docName);
        newDocument.copyComponentViews(this);
        return newDocument;
    }

    private void copyComponentViews(DocumentView documentToCopy) {
        for (DocComponentView docComponentView : getComponentViews()) {
            docComponentView.copyFromDocument(documentToCopy);
        }
    }

    public List<DocumentView> injectDocuments(List<DocumentInjectionInfo> injectionInfos) {

        List<DocumentView> results = new ArrayList<>();

        for (DocumentInjectionInfo injectionInfo : injectionInfos) {
            DocumentView newDocument = copyComponentViews();
            newDocument.setDocumentInjectionFields(injectionInfo);
            injectionInfo.setDocumentNameExtension(newDocument);
            results.add(newDocument);
        }

        return results;
    }

    @Override
    public boolean equals(Object obj) {

        boolean isEqual = false;

        if ((obj instanceof DocumentView)) {

            DocumentView compareTo = (DocumentView) obj;
            EqualsBuilder builder = new EqualsBuilder();

            String thisName = getName();
            String compareToName = compareTo.getName();

            builder.append(thisName, compareToName);

            isEqual = builder.isEquals();
        }

        return isEqual;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        String name = getName();
        builder.append(name);
        return builder.toHashCode();
    }
}
