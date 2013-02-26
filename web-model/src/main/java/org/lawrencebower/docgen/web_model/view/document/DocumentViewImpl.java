package org.lawrencebower.docgen.web_model.view.document;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.Attributes;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class DocumentViewImpl implements DocumentView {

    ViewFactory viewFactory;

    private Document document;
    private List<DocComponentView> docComponentViews = new ArrayList<>();
    private int copyNumber = 1;//default
    private String nameExtension;
    private Attributes customerFilterAttributes = new Attributes();
    private Attributes productFilterAttributes = new Attributes();

    private DocumentViewImpl() {//force spring instantiation
    }

    @Autowired(required = false)
    public void setViewFactory(ViewFactory viewFactory) {
        this.viewFactory = viewFactory;
    }

    @Override
    public void setDocument(Document document) {
        if (document == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

        this.document = document;
    }

    @Override
    public String getName() {
        return document.getName();
    }

    @Override
    public void setNameExtension(String nameExtension) {
        this.nameExtension = nameExtension;
    }

    @Override
    public String getNameExtension() {
        return nameExtension;
    }

    @Override
    public int getCopyNumber() {
        return copyNumber;
    }

    protected void setCopyNumber(int copyNumber) {
        this.copyNumber = copyNumber;
    }

    @Override
    public Document getDocument() {
        return document;
    }

    @Override
    public void addComponentView(DocComponentView componentView) {
        docComponentViews.add(componentView);
    }

    @Override
    public List<DocComponentView> getComponentViews() {
        return docComponentViews;
    }

    /**
     * returns all ComponentViews matching supplied name
     */
    @Override
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

    @Override
    public PDFDocument generatePDF() {
        return document.generatePDF();
    }

    @Override
    public void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo) {
        for (DocComponentView docComponentView : getComponentViews()) {
            docComponentView.setDocumentInjectionFields(injectionInfo);
        }
    }

    @Override
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
    private DocumentView copyComponentViews() {
        String docName = document.getName();
        DocumentView newDocument = viewFactory.createDocument(docName);
        newDocument.copyComponentViews(this);
        return newDocument;
    }

    @Override
    public void copyComponentViews(DocumentView documentToCopy) {
        for (DocComponentView docComponentView : getComponentViews()) {
            docComponentView.copyFromDocument(documentToCopy);
        }
    }

    @Override
    public List<DocumentView> injectDocuments(List<DocumentInjectionInfo> injectionInfos) {

        List<DocumentView> results = new ArrayList<>();

        for (DocumentInjectionInfo injectionInfo : injectionInfos) {
            if (injectionInfo.attributesMatch(this)) {
                DocumentView newDocument = copyComponentViews();
                newDocument.setDocumentInjectionFields(injectionInfo);
                injectionInfo.setDocumentNameExtension(newDocument);
                results.add(newDocument);
            }
        }

        return results;
    }

    protected void setCustomerAttributes(String... attributes){
        customerFilterAttributes = new Attributes(attributes);
    }

    @Override
    public boolean isContactAttributesMatch(ContactView contact) {
        Attributes attributes = contact.getAttributes();
        return customerFilterAttributes.isAttributeMatch(attributes);
    }

    protected void setProductAttributes(String... attributes){
        productFilterAttributes = new Attributes(attributes);
    }

    @Override
    public boolean isProductAttributesMatch(ProductView product) {
        Attributes attributes = product.getAttributes();
        return productFilterAttributes.isAttributeMatch(attributes);
    }

    @Override
    public boolean equals(Object obj) {

        boolean isEqual = false;

        if ((obj instanceof DocumentViewImpl)) {

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
