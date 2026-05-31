"""Sphinx configuration for the lossratio documentation."""
import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(_HERE, "..", "src"))

import lossratio

project = "lossratio"
author = "Seokhoon Joo"
copyright = "2026, Seokhoon Joo"
release = lossratio.__version__
version = release
language = "ko"

extensions = [
    "myst_parser",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon",
    "sphinx.ext.viewcode",
    "sphinx.ext.intersphinx",
    "matplotlib.sphinxext.plot_directive",
]

# matplotlib plot_directive -- run tutorial plot code at build time and
# embed the resulting figure. Keeps code and figures in lockstep so no
# pre-rendered PNGs need to be tracked.
plot_include_source = True
plot_html_show_source_link = False
plot_html_show_formats = False
plot_formats = [("png", 144)]

# sphinx-copybutton -- strip Python REPL and shell prompts on copy.
# `# ` is *not* a prompt here because it doubles as the Python comment
# marker -- including it would also strip section-header comment lines
# from copied code.
copybutton_prompt_text = r">>> |\.\.\. |\$ "
copybutton_prompt_is_regexp = True

templates_path = ["_templates"]

autodoc_member_order = "bysource"
autodoc_typehints = "description"

intersphinx_mapping = {
    "python": ("https://docs.python.org/3", None),
    "numpy": ("https://numpy.org/doc/stable", None),
    "polars": ("https://docs.pola.rs/api/python/stable", None),
}

myst_enable_extensions = ["colon_fence", "deflist", "substitution", "dollarmath"]

exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

html_theme = "pydata_sphinx_theme"
html_title = "lossratio"
html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_show_sourcelink = False
html_copy_source = False

html_theme_options = {
    "show_prev_next": True,
    "navbar_align": "content",
    "external_links": [
        {"name": "데모", "url": "https://demo.lossratio.org"},
    ],
    "icon_links": [
        {
            "name": "GitHub",
            "url": "https://github.com/seokhoonj/lossratio",
            "icon": "fa-brands fa-github",
        },
        {
            "name": "PyPI",
            "url": "https://pypi.org/project/lossratio/",
            "icon": "fa-brands fa-python",
        },
    ],
}
