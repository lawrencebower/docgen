package org.lawrencebower.docgen.core.document.component;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

import static junit.framework.Assert.assertEquals;

public class DocComponentTest {

    @Test
    public void testGetPosition_defaultValue_isLeft() throws Exception {

        DocComponent genericDocComponent = new AbstractDocComponent() {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };

        HorizontalAlignment defaultAlignment = genericDocComponent.getAlignment();

        assertEquals(HorizontalAlignment.LEFT, defaultAlignment);
    }
}
