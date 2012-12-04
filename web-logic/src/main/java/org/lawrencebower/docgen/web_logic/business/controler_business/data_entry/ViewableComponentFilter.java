package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class ViewableComponentFilter {

    public List<DocComponentView> getComponents(List<DocComponentView> allComponents) {

        List<DocComponentView> results = filterDuplicatedFields(allComponents);

        return results;
    }

    public List<DocComponentView> getNonAutoMappedComponents(List<DocComponentView> allComponents) {

        List<DocComponentView> results = filterAutomapped(allComponents);

        results = filterDuplicatedFields(results);

        return results;
    }

    private List<DocComponentView> filterAutomapped(List<DocComponentView> componentViews) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocComponentView docComponentView : componentViews) {
            if (docComponentView.isAutoMapped()) {
                continue;//skip to the next one
            }
            results.add(docComponentView);

        }
        return results;
    }

    /**
     * Adds the DocComponentViews to a Set and returns a unique list in the order they were added.
     * There will be one DocComponent with each unique component name in the set. The Set evaluates
     * the equality of the DocComponentViews based on the DocComponentView names.
     */
    private ArrayList<DocComponentView> filterDuplicatedFields(List<DocComponentView> documents) {

        LinkedHashSet<DocComponentView> filteredViews = new LinkedHashSet<>();
        for (DocComponentView document : documents) {
            filteredViews.add(document);
        }

        return new ArrayList<>(filteredViews);
    }
}
