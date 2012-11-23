package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;

import java.util.List;

public interface ComponentCalculator<T extends DocComponentView, T2 extends ComponentCalculation> {

    void runCalculation(T componentView,
                        T2 calculation,
                        List<DocumentInfoView> allDocs);
}
