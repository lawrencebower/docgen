package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public abstract class AbstractAMComponent implements AutoMapped {

    protected String name;

    public String getName() {
        return name;
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
        return this.name.equals(name);
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
