package org.lawrencebower.docgen.web_logic.view.document;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.List;

public interface ViewableComponentFilter {
    List<DocComponentView> getComponents(List<DocComponentView> allComponents);

    List<DocComponentView> getNonAutoMappedComponents(List<DocComponentView> allComponents);
}
