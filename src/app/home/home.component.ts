import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Territory } from '../territory';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  territories: Territory[] = [];

  constructor(private http: HttpClient) { }

  ngOnInit() {
    this.http.get('/api/territories').subscribe((territories: any[]) => {
      for (const terr of territories) {
        terr.boundary = JSON.parse(terr.boundary);
      }
      this.territories = territories;
    });
  }

  newTerritory() {

  }

}
