package org.lawrencebower.docgen.web_model.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public class DocumentInfoView {

    private DocumentInfo documentInfo;
    private List<DocComponentView> docComponentViews;

    private boolean isDirty = false;
    private boolean isComplete = false;

    public DocumentInfoView(DocumentInfo documentInfo) {
        if (documentInfo == null) {
            throw new DocGenException("DocumentInfo parameter is null");
        }

        docComponentViews = new ArrayList<>();

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

    public DocumentInfo createDocumentInfo() {

        return documentInfo;
    }

/*
    public List<DocComponentView> getEditableFields() {

        List<DocComponentView> editableViews = new ArrayList<>();

        for (DocComponentView docComponentView : docComponentViews) {
            if(docComponentView.isEditable()){
                editableViews.add(docComponentView);
            }
        }

        return editableViews;
    }
*/

    public void addComponentView(DocComponent component, DocComponentView.ComponentViewType viewType) {
        DocComponentView componentView = new DocComponentView(component, viewType);
        docComponentViews.add(componentView);
    }

    public void addComponentView(DocComponentView componentView) {
        docComponentViews.add(componentView);
    }

    public List<DocComponentView> getComponentViews() {
        return docComponentViews;
    }
}
