package org.lawrencebower.docgen.web_logic.view.document_info;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DocumentInfoSet {

    private List<DocumentInfoView> documents = new ArrayList<>();

    private DocumentInfoSet() {//force spring creation
    }

    public void setDocuments(List<DocumentInfoView> documents) {
        this.documents = documents;
    }

    public void setDocuments(DocumentInfoView... views) {
        List<DocumentInfoView> docList = Arrays.asList(views);
        documents = new ArrayList<>(docList);
    }

}
