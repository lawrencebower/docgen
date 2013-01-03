package org.lawrencebower.docgen.web_model.view.document;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.table.TableComponentCalculation;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewFactory;
import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;
import org.springframework.beans.factory.annotation.Autowired;

public class DocumentViewBuilder {

    @Autowired
    private DocumentViewFactory viewFactory;
    @Autowired
    private DocComponentViewFactory componentViewFactory;

    private DocumentViewImpl docView;

    public void createDocument() {
        docView = viewFactory.createDocumentView();
    }

    public DocumentViewImpl getDocumentView() {
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
