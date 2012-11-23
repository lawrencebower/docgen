package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponent;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public abstract class DocComponentView<T extends DocComponent> {

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
    private AutoMappedComponent autoMappedComponent;

    public void setComponent(T docComponent){
        if (docComponent == null) {
            throw new DocGenException(NULL_COMPONENT_MESSAGE);
        }

        this.docComponent = docComponent;
    }

    public DocComponent getDocComponent() {
        return docComponent;
    }

    public String getName() {

        String name = NOT_SET_MESSAGE;

        if(docComponent.getName() != null){
            name = docComponent.getName();
        }

        return name;
    }

    public abstract void setComponentValue(Boolean value);

    public abstract void setComponentValue(String value);

    public abstract void setComponentValue(Float value);

    public abstract String getStringValue();

    public abstract Float getFloatValue();

    public abstract Boolean getBooleanValue();

    public abstract void checkAndSetValueFromParamString(String componentName, String value);

    public abstract void injectProducts(List<ProductView> products);

    public boolean isAutoMappedComponent(){
        return autoMappedComponent != null;
    }

    public void setAutoMappedComponent(AutoMappedComponent autoMappedComponent) {
        this.autoMappedComponent = autoMappedComponent;
    }

    public void mapComponentValue(AutoMappedComponentInfo mappingInfo) {
        if(isAutoMappedComponent()){
            AutoMappedComponent.mapComponent(this,
                                             autoMappedComponent,
                                             mappingInfo);

        }
    }

    public abstract boolean hasCalculation();

    public abstract void calculateValueIfNeeded(List<DocumentInfoView> allDocs);

    public boolean isText(){
        return componentViewType == ComponentViewType.TEXT;
    }

    public boolean isTextArea(){
        return componentViewType == ComponentViewType.TEXT_AREA;
    }

    public boolean isTable(){
        return componentViewType == ComponentViewType.TABLE;
    }

    @Override
    public boolean equals(Object obj) {

        if(!(obj instanceof DocComponentView)){
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
