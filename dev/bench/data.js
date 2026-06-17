window.BENCHMARK_DATA = {
  "lastUpdate": 1781724534874,
  "repoUrl": "https://github.com/Sealybla/jsip-exchange",
  "entries": {
    "Order book benchmark": [
      {
        "commit": {
          "author": {
            "email": "139495621+Sealybla@users.noreply.github.com",
            "name": "Sealybla",
            "username": "Sealybla"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c2e3db6bf855faae68fb223844131d4f3cad0d96",
          "message": "Merge branch 'jane-street-immersion-program:main' into main",
          "timestamp": "2026-06-17T15:24:55-04:00",
          "tree_id": "b105f708f1d0a3bfac0fc8f703926fc5cb5958f3",
          "url": "https://github.com/Sealybla/jsip-exchange/commit/c2e3db6bf855faae68fb223844131d4f3cad0d96"
        },
        "date": 1781724534403,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "find_match (n=10)",
            "value": 25.710263279806874,
            "unit": "ns"
          },
          {
            "name": "find_match (n=50)",
            "value": 24.528051598437543,
            "unit": "ns"
          },
          {
            "name": "find_match (n=100)",
            "value": 24.40903152032214,
            "unit": "ns"
          },
          {
            "name": "find_match (n=500)",
            "value": 24.576809796202756,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=10)",
            "value": 115.70118673628325,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=50)",
            "value": 511.64602232012345,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=100)",
            "value": 1080.9168323819918,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=500)",
            "value": 5662.433039942375,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=10)",
            "value": 235.68951792124076,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=50)",
            "value": 1108.8891772113197,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=100)",
            "value": 2201.0842670440984,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=500)",
            "value": 10933.77645717826,
            "unit": "ns"
          },
          {
            "name": "add+remove (n=100)",
            "value": 1731.5101427703976,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=10)",
            "value": 1344.7586936734544,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=50)",
            "value": 5565.565300368291,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=100)",
            "value": 10583.457942962996,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=500)",
            "value": 50439.5821806163,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=10)",
            "value": 664.0252021114846,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=50)",
            "value": 2836.8198200206057,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=100)",
            "value": 5570.670987505246,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=500)",
            "value": 26250.440626281175,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_10_levels",
            "value": 5732.358470173915,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_50_levels",
            "value": 86127.3069921074,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_100_levels",
            "value": 321010.8300589971,
            "unit": "ns"
          },
          {
            "name": "find_match_alloc (n=100)",
            "value": 27.5919678623611,
            "unit": "ns"
          }
        ]
      }
    ]
  }
}