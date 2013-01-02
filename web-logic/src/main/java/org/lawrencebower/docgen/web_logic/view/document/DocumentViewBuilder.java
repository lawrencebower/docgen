package org.lawrencebower.docgen.web_logic.view.document;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.*;
import org.springframework.beans.factory.annotation.Autowired;

public class DocumentViewBuilder {

    @Autowired
    private DocumentViewFactory viewFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;

    private DocumentView docView;

    public void createDocument() {
        docView = viewFactory.createDocumentView();
    }

    public DocumentView getDocumentView() {
        return docView;
    }

    public DocComponentView addViewableComponent(DocComponent component) {
        DocComponentView componentView = componentViewFactory.createComponentView(component);
        docView.addComponentView(componentView);
        return componentView;
    }

    public void addViewableComponent(DocComponent component,
                                     ComponentCalculation calculation) {

        DocComponentView componentView = addViewableComponent(component);
        componentView.setComponentCalculation(calculation);
    }

    public void addViewableComponent(TableComponent tableComponent,
                                     TableComponentCalculation... calculations) {

        TableComponentView componentView = (TableComponentView) addViewableComponent(tableComponent);

        for (TableComponentCalculation calculation : calculations) {
            componentView.addComponentCalculation(calculation);
        }
    }
}
