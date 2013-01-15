package org.lawrencebower.docgen.web_model.view.document.component;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface DocComponentView<T extends DocComponent> {

    void setComponentValue(Boolean value);

    void setComponentValue(String value);

    String getStringValue();

    Boolean getBooleanValue();

    void checkAndSetValueFromParamString(String componentName, String value);

    void injectProducts(List<ProductView> products);

    void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo);

    boolean hasCalculation();

    void setComponentCalculation(ComponentCalculation calculation);

    void calculateValueIfNeeded(DocumentSet documentSet);

    boolean runCalculationIfMatch(String operand,
                                  ComponentCalculation calculation,
                                  DocumentSet documentSet);

    void copyFromDocument(DocumentView documentToCopy);

    boolean isDocumentInjection();

    void setComponent(T docComponent);

    String getName();

    void mapComponentValue(AMComponentInfo mappingInfo);

    boolean isAutoMapped();

    boolean isText();

    boolean isTextArea();

    boolean isTable();

    void setRenderBorder(boolean renderBorder);
}
