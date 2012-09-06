package org.lawrencebower.docgen.core.document.component;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

import static junit.framework.Assert.assertEquals;

public class DocComponentTest {

    @Test
    public void testGetPosition_defaultValue_isLeft() throws Exception {

        DocComponent genericDocComponent = new DocComponent() {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };

        DocPosition defaultPosition = genericDocComponent.getPosition();
        HorizontalAlignment defaultAlignment = defaultPosition.getHorizontalAlignment();

        assertEquals(HorizontalAlignment.LEFT, defaultAlignment);
    }
}
