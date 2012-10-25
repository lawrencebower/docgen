package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.ArrayList;
import java.util.List;

public class ViewUtils {

    public List<DocComponentView> getAllComponentViewsFromDocs(List<DocumentInfoView> documents) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocumentInfoView documentInfoView : documents) {
            for (DocComponentView docComponentView : documentInfoView.getComponentViews()) {
                results.add(docComponentView);
            }
        }

        return results;
    }
}
