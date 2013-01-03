package org.lawrencebower.docgen.web_logic.view.document.component;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_logic.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public abstract class DocComponentView<T extends DocComponent> {

    @Autowired(required = false)
    private AutoMapped autoMapper;

    protected static final String NULL_COMPONENT_MESSAGE = "DocComponent is null";
    protected static final String NOT_SET_MESSAGE = "not set";

    public enum ComponentViewType {
        TEXT,
        TEXT_AREA,
        CHECK_BOX,
        TABLE,
        CV
    }

    protected T docComponent;
    protected ComponentViewType componentViewType;

    public abstract void setComponentValue(Boolean value);

    public abstract void setComponentValue(String value);

    public abstract void setComponentValue(Float value);

    public abstract String getStringValue();

    public abstract Float getFloatValue();

    public abstract Boolean getBooleanValue();

    public abstract void checkAndSetValueFromParamString(String componentName, String value);

    public abstract void injectProducts(List<ProductView> products);

    public abstract void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo);

    public abstract boolean hasCalculation();

    public abstract void setComponentCalculation(ComponentCalculation calculation);

    public abstract void calculateValueIfNeeded(DocumentSet documentSet);

    public abstract boolean runCalculationIfMatch(String operand,
                                                  ComponentCalculation calculation,
                                                  DocumentSet documentSet);

    public abstract void copyFromDocument(DocumentView documentToCopy);

    public boolean isDocumentInjection() {
        String componentName = getName();
        return DocumentInjectionField.containsName(componentName);
    }

    public void setComponent(T docComponent) {
        if (docComponent == null) {
            throw new DocGenException(NULL_COMPONENT_MESSAGE);
        }

        this.docComponent = docComponent;
    }

    public String getName() {

        String name = NOT_SET_MESSAGE;

        if (docComponent.getName() != null) {
            name = docComponent.getName();
        }

        return name;
    }

    public void mapComponentValue(AMComponentInfo mappingInfo) {
        if (isAutoMapped()) {
            autoMapper.mapComponent(this, mappingInfo);
        }
    }

    public boolean isAutoMapped(){
        String componentName = getName();
        return autoMapper.matchesName(componentName);
    }

    public boolean isText() {
        return componentViewType == ComponentViewType.TEXT;
    }

    public boolean isTextArea() {
        return componentViewType == ComponentViewType.TEXT_AREA;
    }

    public boolean isTable() {
        return componentViewType == ComponentViewType.TABLE;
    }

    public void setRenderBorder(boolean renderBorder) {
        docComponent.setRenderBorder(renderBorder);
    }

    @Override
    public boolean equals(Object obj) {

        if (!(obj instanceof DocComponentView)) {
            return false;
        }

        DocComponentView compareTo = (DocComponentView) obj;
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(this.getName(), compareTo.getName());

        return builder.isEquals();
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(this.getName());
        return builder.toHashCode();
    }
}
