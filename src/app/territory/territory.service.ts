import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs/observable';
import 'rxjs/add/operator/map';

import { Territory } from '../territory';

@Injectable()
export class TerritoryService {

  territories: Territory[];

  constructor(private http: HttpClient) { }

  public getAll(): Observable<Territory[]> {
    return this.territories
      ? Observable.of(this.territories)
      : this.http.get('/api/territories').map((territories: any[]) => {
          for (const terr of territories) {
            terr.boundary = JSON.parse(terr.boundary);
          }
          this.territories = territories;
          return territories;
        });
  }

  public getByName(name: string): Observable<Territory> {
    const cachedTerritory = (this.territories || []).find((terr: Territory) => {
      return terr.name === name;
    });
    return (cachedTerritory)
      ? Observable.of(cachedTerritory)
      : this.http.get(`/api/territory/${name}`).map((territory: any) => {
          territory.boundary = JSON.parse(territory.boundary);
          return territory;
        });
  }

  public save(territory: Territory): Observable<string> {
    const boundary = JSON.stringify(territory.boundary);
    const territoryToSave = Object.assign({}, territory, { boundary });
    return this.http.post('/api/saveTerritory', JSON.stringify(territoryToSave), {
      headers: { 'Content-Type': 'application/json' },
      responseType: 'text'
    });
  }

  public copyTree(originalTree: {subs}[]): {data, subs}[] {
    return originalTree.map(({ subs, ...data }) => ({
      data,
      subs: this.copyTree(subs)
    }));
  }

}
