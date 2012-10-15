package org.lawrencebower.docgen.web_model.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public class DocumentInfoView {

    private DocumentInfo documentInfo;
    private List<DocComponentView> docComponents;

    private boolean isDirty = false;
    private boolean isComplete = false;

    public DocumentInfoView(DocumentInfo documentInfo) {
        if (documentInfo == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

        docComponents = new ArrayList<>();

        this.documentInfo = documentInfo;
    }

    public boolean isDirty() {
        return isDirty;
    }

    public boolean isComplete() {
        return isComplete;
    }

    public String getName() {
        return documentInfo.getName();
    }

    public DocumentInfo getDocument() {

        List<DocComponent> components = new ArrayList<>();

        for (DocComponentView docComponentView : docComponents) {
            components.add(docComponentView.getDocComponent());
        }

        documentInfo.setComponents(components);

        return documentInfo;
    }

    public List<DocComponentView> getEditableFields() {
        return docComponents;
    }

    public void addComponent(DocComponent component, boolean editable) {
        DocComponentView componentView = new DocComponentView(component);
        componentView.setEditable(editable);
        docComponents.add(componentView);
    }

    public List<DocComponentView> getComponents() {
        return docComponents;
    }
}
