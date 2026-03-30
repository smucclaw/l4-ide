#!/usr/bin/env python3
"""
fibo_sparql.py — Lightweight SPARQL endpoint for FIBO regulatory data.

Loads FIBO RDF ontology files (from Elisa's SEC-215 branch) into an
in-memory rdflib graph and serves SPARQL queries via a Flask HTTP endpoint.

Usage:
    python3 fibo_sparql.py [--port 7878] [--fibo-dir /path/to/fibo]

Endpoints:
    GET  /sparql?query=...  — SPARQL query (URL-encoded)
    POST /sparql             — SPARQL query in request body (application/sparql-query)
                               or as form field 'query' (application/x-www-form-urlencoded)
    GET  /                   — Simple status page listing loaded triples and LEIs

Example:
    curl -G http://localhost:7878/sparql --data-urlencode 'query=
      PREFIX fibo-fnd-rel-rel: <https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/>
      PREFIX cmns-id: <https://www.omg.org/spec/Commons/Identifiers/>
      SELECT ?lei ?name ?regulator WHERE {
        ?leiIRI fibo-fnd-rel-rel:hasTag ?lei .
        ?leiIRI cmns-id:identifies ?entity .
        ?entity rdfs:label ?name .
        ?funcEntity owl:sameAs ?entity .
        ?funcEntity <https://spec.edmcouncil.org/fibo/ontology/FBC/FunctionalEntities/FinancialServicesEntities/isRegulatedBy> ?reg .
        ?reg rdfs:label ?regulator .
      }'
"""

import argparse
import json
import os
import sys
from pathlib import Path

try:
    import rdflib
except ImportError:
    sys.exit("rdflib not installed. Run: pip install rdflib")

try:
    from flask import Flask, request, jsonify, Response
except ImportError:
    sys.exit("flask not installed. Run: pip install flask")


# Default FIBO directory
DEFAULT_FIBO_DIR = os.environ.get(
    "FIBO_DIR",
    str(Path(__file__).parent.parent.parent.parent / "edmcouncil" / "fibo"),
)

# RDF files to load (relative to FIBO root)
# These are the files from SEC-215 that contain the regulatory data
FIBO_RDF_FILES = [
    "FBC/FunctionalEntities/EuropeanEntities/EuropeanFinancialServicesEntitiesIndividuals.rdf",
    "FBC/FunctionalEntities/EuropeanEntities/EURegulatoryAgencies.rdf",
    "FBC/FunctionalEntities/NorthAmericanEntities/USFinancialServicesEntitiesIndividuals.rdf",
    "FBC/FunctionalEntities/NorthAmericanEntities/USRegulatoryAgencies.rdf",
    "ACTUS/ACTUS-examples.ttl",
]


def load_fibo_graph(fibo_dir: str) -> rdflib.Graph:
    """Load FIBO RDF files into an in-memory graph."""
    g = rdflib.Graph()

    for rdf_file in FIBO_RDF_FILES:
        path = Path(fibo_dir) / rdf_file
        if path.exists():
            fmt = "turtle" if path.suffix == ".ttl" else "xml"
            try:
                g.parse(str(path), format=fmt)
                print(f"  Loaded {path.name} ({fmt})")
            except Exception as e:
                print(f"  Warning: Failed to parse {path.name}: {e}", file=sys.stderr)
        else:
            print(f"  Skipped {rdf_file} (not found)", file=sys.stderr)

    print(f"  Total: {len(g)} triples")
    return g


def create_app(graph: rdflib.Graph) -> Flask:
    """Create Flask app with SPARQL endpoint."""
    app = Flask(__name__)

    def _run_sparql(query_str):
        """Execute a SPARQL query and return a Flask Response."""
        if not query_str.strip():
            return jsonify({"error": "No query provided"}), 400

        try:
            results = graph.query(query_str)
        except Exception as e:
            return jsonify({"error": str(e)}), 400

        if results.type == "SELECT":
            bindings = []
            for row in results:
                binding = {}
                for var in results.vars:
                    val = getattr(row, str(var), None)
                    if val is not None:
                        binding[str(var)] = {
                            "type": "uri" if isinstance(val, rdflib.URIRef) else "literal",
                            "value": str(val),
                        }
                bindings.append(binding)

            resp = {
                "head": {"vars": [str(v) for v in results.vars]},
                "results": {"bindings": bindings},
            }
            return Response(
                json.dumps(resp, indent=2),
                mimetype="application/sparql-results+json",
            )
        elif results.type == "ASK":
            return jsonify({"head": {}, "boolean": bool(results)})
        elif results.type == "CONSTRUCT":
            return Response(
                results.serialize(format="turtle"),
                mimetype="text/turtle",
            )
        else:
            return jsonify({"error": f"Unsupported query type: {results.type}"}), 400

    def _extract_query():
        """Extract SPARQL query from GET params or POST body."""
        if request.method == "GET":
            return request.args.get("query", "")
        ct = request.content_type or ""
        if "sparql-query" in ct:
            return request.get_data(as_text=True)
        elif "x-www-form-urlencoded" in ct:
            return request.form.get("query", "")
        elif "json" in ct:
            data = request.get_json(silent=True) or {}
            return data.get("query", "")
        return request.get_data(as_text=True)

    @app.route("/", methods=["GET", "POST"])
    def index():
        # If a query parameter is present, treat as SPARQL query
        # This allows https://dev.jl4.legalese.com/sparql/?query=... to work
        if request.args.get("query") or request.method == "POST":
            return _run_sparql(_extract_query())

        # Otherwise show loaded LEIs and basic stats
        lei_query = """
        PREFIX fibo-fnd-rel-rel: <https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/>
        PREFIX cmns-id: <https://www.omg.org/spec/Commons/Identifiers/>
        SELECT ?lei ?name WHERE {
            ?leiIRI fibo-fnd-rel-rel:hasTag ?lei .
            ?leiIRI cmns-id:identifies ?entity .
            ?entity rdfs:label ?name .
        } ORDER BY ?name
        """
        results = graph.query(lei_query)
        leis = [{"lei": str(row.lei), "name": str(row.name)} for row in results]

        return jsonify({
            "status": "ok",
            "triples": len(graph),
            "entities": len(leis),
            "leis": leis,
            "endpoints": {
                "sparql_get": "GET ?query=... (URL-encoded SPARQL)",
                "sparql_post": "POST (Content-Type: application/sparql-query)",
            },
        })

    @app.route("/sparql", methods=["GET", "POST"])
    def sparql():
        return _run_sparql(_extract_query())

    @app.route("/sparql/", methods=["GET", "POST"])
    def sparql_slash():
        return _run_sparql(_extract_query())

    return app


def main():
    parser = argparse.ArgumentParser(description="FIBO SPARQL endpoint")
    parser.add_argument("--port", type=int, default=7878, help="Port to listen on")
    parser.add_argument("--host", default="0.0.0.0", help="Host to bind to")
    parser.add_argument("--fibo-dir", default=DEFAULT_FIBO_DIR, help="Path to FIBO repo")
    args = parser.parse_args()

    print(f"Loading FIBO data from {args.fibo_dir}...")
    graph = load_fibo_graph(args.fibo_dir)

    print(f"\nStarting SPARQL endpoint on http://localhost:{args.port}/sparql")
    app = create_app(graph)
    app.run(host=args.host, port=args.port, debug=False)


if __name__ == "__main__":
    main()
