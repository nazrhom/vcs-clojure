("Create a tag object. Note that this does not create the reference
 that makes a tag in Git. If you want to create an annotated tag, you
 have to do this call to create the tag object and then create the
 `refs/tags/[tag]` reference. If you want to create a lightweight tag,
 you simply need to create the reference and this call would be
 unnecessary.
 Options are:
    tagger -- A map (string keys) containing the following:
              \"name\"  -- Name of the author of this tag.
              \"email\" -- Email of the author of this tag.
              \"date\"  -- Timestamp when this object was tagged.")
