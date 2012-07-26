package org.lawrencebower.docgen.core.document.component;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;

import static junit.framework.Assert.assertEquals;

public class DocComponentTest {

    @Test
    public void testGetPosition_defaultValue_isLeft() throws Exception {

        DocComponent genericDocComponent = new DocComponent("name") {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };

        DocPosition defaultPosition = genericDocComponent.getPosition();
        DocAlignment defaultAlignment = defaultPosition.getAlignment();

        assertEquals(DocAlignment.LEFT, defaultAlignment);
    }
}
