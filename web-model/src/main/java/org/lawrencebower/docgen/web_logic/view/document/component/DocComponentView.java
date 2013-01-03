package org.lawrencebower.docgen.web_logic.view.document.component;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_logic.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public interface DocComponentView<T extends DocComponent> {

    void setComponentValue(Boolean value);

    void setComponentValue(String value);

    void setComponentValue(Float value);

    String getStringValue();

    Float getFloatValue();

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
