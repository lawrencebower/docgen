package org.lawrencebower.docgen.core.document;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public class ComponentBuilder {

    public NewLineComponent createNewLine() {
        return new NewLineComponent();
    }

    public TextComponent createTextComponentWithName(String name) {
        return createTextComponent(name,
                                   "",
                                   HorizontalAlignment.LEFT);
    }

    public TextComponent createTextComponentWithValue(String value) {
        return createTextComponent(null,
                                   value,
                                   HorizontalAlignment.LEFT);
    }

    public TextComponent createTextComponent(String name,
                                             String value) {
        return createTextComponent(name,
                                   value,
                                   HorizontalAlignment.LEFT);
    }

    public TextComponent createTextComponent(String name,
                                             String value,
                                             HorizontalAlignment alignment) {

        TextComponent textComponent = new TextComponent(value);

        setNameIfSet(name, textComponent);

        textComponent.setAlignment(alignment);

        return textComponent;
    }

    private void setNameIfSet(String name, DocComponent component) {
        if (!StringUtils.isWhitespace(name)) {
            component.setName(name);
        }
    }

    public TableTextComponent createTableTextComponent(String name, String value) {
        TextComponent textComponent = createTextComponent(name, value);
        return new TableTextComponent(textComponent);
    }

    public TableTextComponent createTableTextComponentWithName(String name) {
        TextComponent textComponent = createTextComponentWithName(name);
        return new TableTextComponent(textComponent);
    }
}
