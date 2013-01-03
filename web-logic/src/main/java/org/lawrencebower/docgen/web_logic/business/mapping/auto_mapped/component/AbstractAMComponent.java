package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public abstract class AbstractAMComponent implements AutoMapped {

    protected AutoMappedField name;

    public String getName() {
        return name.getName();
    }

    protected boolean setComponentValueIfMatch(DocComponentView docComponent,
                                               String value) {

        String componentName = docComponent.getName();

        boolean componentMatched = false;

        if (matchesName(componentName)) {
            setComponentText(docComponent, value);
            componentMatched = true;
        }

        return componentMatched;
    }

    @Override
    public boolean matchesName(String name) {
        String nameString = getName();
        return nameString.equals(name);
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
